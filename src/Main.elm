port module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html, Attribute, h1, div, span, text, button, input)
import Html.Attributes exposing (class, id, style, attribute, value, disabled)
import Html.Events exposing (onClick, onInput, on, stopPropagationOn, keyCode)
import Svg exposing (svg, line, text_) -- "text_" is an element, "text" is a node
import Svg.Attributes exposing (viewBox, width, height, x, y, x1, y1, x2, y2, stroke, fill)
import Dict exposing (Dict)
import Array
import Json.Decode as D
import Json.Encode as E
import Task exposing (Task)
import Debug exposing (log)



-- PORTS


port store : E.Value -> Cmd msg



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
  ( case D.decodeValue decoder flags of
      Ok model ->
        log "Reading localStorage" model
      Err e ->
        let
          _ = logError "init" "Could not read localStorage" e
        in
        defaultModel
  , Cmd.none
  )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  {--}
  let
    _ = log "update" msg
  in
  --}
  case msg of
    AddTimeline -> addTimeline model |> updateAndStore
    AddTimespan tlId point size result ->
      addTimespan model tlId point size result |> updateAndStore
    Select target -> ( { model | selection = target }, Cmd.none )
    EditStart target -> ( { model | editState = target }, Cmd.none )
    Edit title -> updateTitle model title |> updateAndStore
    EditEnd -> ( { model | editState = NoEdit }, Cmd.none )
    MouseDown -> ( reset model, Cmd.none )
    MouseDownItem p class id -> ( mouseDownOnItem model p class id, Cmd.none )
    MouseMove p -> ( mouseMove model p, Cmd.none )
    MouseUp -> mouseUp model
    Delete -> delete model |> updateAndStore
    -- Zooming
    Zoom op -> zoom model op
    -- Settings dialog
    OpenSettings -> ( { model | isSettingsOpen = True }, Cmd.none )
    EditSetting field value -> ( updateSetting model field value, Cmd.none )
    CloseSettings -> closeSettings model |> updateAndStore
    --
    NoOp -> ( model, Cmd.none )


updateAndStore : Model -> ( Model, Cmd Msg )
updateAndStore model =
  ( model, encode model |> store )


addTimeline : Model -> Model
addTimeline model =
  let
    id = model.nextId
    colorIndex = modBy (Array.length conf.timelineColors) (Dict.size model.timelines)
    color = case conf.timelineColors |> Array.get colorIndex of
      Just color_ -> color_
      Nothing -> logError "addTimeline" "can't compute timeline color" 0
  in
    { model
      | timelines = model.timelines |> Dict.insert id
        ( Timeline id "New Timeline" color [] )
      , editState = TimelineEdit id
      , nextId = id + 1
    }


addTimespan : Model -> Id -> Point -> Size -> Result Dom.Error Dom.Element -> Model
addTimespan model tlId point size result =
  case result of
    Ok timelinesDom ->
      let
        tsId = model.nextId
        x = point.x - round timelinesDom.element.x
        ( begin, end ) = toModelSpace model x size.width
        timespan = Timespan tsId "New Timespan" begin end
      in
      { model
        | timelines = updateTimeline model tlId tsId
        , timespans = model.timespans |> Dict.insert tsId timespan
        , selection = TimespanSelection tsId
        , editState = TimespanEdit tsId
        , nextId = model.nextId + 1
      }
    Err (Dom.NotFound e) -> logError "addTimespan" ("Dom.NotFound \"" ++ e ++ "\"") model


mouseDownOnItem : Model -> Point -> Class -> Id -> Model
mouseDownOnItem model p class id =
  let
    newModel = reset model
  in
  { newModel | dragState = Engaged p class id }


mouseMove : Model -> Point -> Model
mouseMove model p =
  case model.dragState of
    Engaged p_ class id ->
      let
        dragState = case class of
          "tl-timespan"      -> DragTimespan id MoveTimespan p_
          "tl-resizer-left"  -> DragTimespan id MoveBegin p_
          "tl-resizer-right" -> DragTimespan id MoveEnd p_
          "tl-timeline" -> DrawRect id p_ (Size 0 0)
          _ -> NoDrag
      in
      performDrag { model | dragState = dragState } p
    DragTimespan _ _ _ ->
      performDrag model p
    DrawRect _ _ _ ->
      performDrag model p
    NoDrag ->
      logError "mouseMove" "Received MouseMove message when dragState is NoDrag" model


performDrag : Model -> Point -> Model
performDrag model p =
  case model.dragState of
    DragTimespan id mode lastPoint ->
      let
        delta = p.x - lastPoint.x
      in
      { model
        | timespans = updateTimespan model id delta mode
        , dragState = DragTimespan id mode p -- update lastPoint
      }
    DrawRect id startPoint size ->
      let
        w = p.x - startPoint.x
        h = p.y - startPoint.y
      in
      { model | dragState = DrawRect id startPoint (Size w h) } -- update size
    Engaged _ _ _ ->
      logError "performDrag" "Received MouseMove message when dragState is Engaged" model
    NoDrag ->
      logError "performDrag" "Received MouseMove message when dragState is NoDrag" model


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  ( { model | dragState = NoDrag }
  , case model.dragState of
      NoDrag -> logError "mouseUp" "Received MouseUp when dragState is NoDrag" Cmd.none
      Engaged _ _ _ -> log "Hint: MouseUp without dragging" Cmd.none
      DragTimespan _ _ _ -> encode model |> store
      DrawRect tlId point size ->
        Task.attempt (AddTimespan tlId point size) (Dom.getElement "tl-timelines")
  )


updateTimespan : Model -> Id -> Int -> TimespanMode -> Timespans
updateTimespan model id delta mode =
  let
    delta_ = toModelValue model delta
  in
  Dict.update
    id
    (\ts -> case ts of
      Just timespan -> Just <|
        case mode of
          MoveTimespan ->
            { timespan
              | begin = timespan.begin + delta_
              , end = timespan.end + delta_
            }
          MoveBegin ->
            { timespan | begin = timespan.begin + delta_ }
          MoveEnd ->
            { timespan | end = timespan.end + delta_ }
      Nothing -> illegalTimespanId "updateTimespan" id Nothing
    )
    model.timespans


updateTimeline : Model -> Id -> Id -> Timelines
updateTimeline model tlId tsId =
  Dict.update
    tlId
    (\tl -> case tl of
      Just timeline -> Just { timeline | tsIds = tsId :: timeline.tsIds }
      Nothing -> illegalTimelineId "updateTimeline" tlId Nothing
    )
    model.timelines


updateTitle : Model -> String -> Model
updateTitle model title =
  case model.editState of
    TimelineEdit id ->
      { model | timelines = updateTitleDict model.timelines "Timeline" id title }
    TimespanEdit id ->
      { model | timespans = updateTitleDict model.timespans "Timespan" id title }
    TitleEdit -> { model | title = title }
    NoEdit -> logError "updateTitle" "called when editState is NoEdit" model


updateTitleDict : TitleDict a -> String -> Id -> String -> TitleDict a
updateTitleDict dict item id title =
  dict |> Dict.update id
    (\value_ -> case value_ of
      Just value -> Just { value | title = title }
      Nothing -> illegalId "updateTitleDict" item id Nothing
    )


delete : Model -> Model
delete model =
  case model.selection of
    TimelineSelection id ->
      case model.timelines |> Dict.get id of
        Just timeline ->
          { model
            | timelines = model.timelines |> Dict.remove id
            , timespans = model.timespans |> Dict.filter
                (\tsId timespan -> not (List.member tsId timeline.tsIds))
            , selection = NoSelection
          }
        Nothing -> illegalTimelineId "delete" id model
    TimespanSelection id ->
      case model.timespans |> Dict.get id of
        Just timespan ->
          { model
            | timelines = removeFromTimeline model id
            , timespans = model.timespans |> Dict.remove id
            , selection = NoSelection
          }
        Nothing -> illegalTimespanId "delete" id model
    NoSelection -> logError "delete" "called when \"selection\" state is NoSelection" model


removeFromTimeline : Model -> Id -> Timelines
removeFromTimeline model tsId =
  case findTimelineOfTimespan model tsId of
    Just tlId ->
      model.timelines |> Dict.update tlId
        (\tl -> case tl of
          Just timeline -> Just
            { timeline | tsIds = timeline.tsIds
              |> List.filter (\tsId_ -> tsId_ /= tsId)
            }
          Nothing -> Nothing -- error logged already
        )
    Nothing -> model.timelines -- error logged already


findTimelineOfTimespan : Model -> Id -> Maybe Id
findTimelineOfTimespan model tsId =
  let
    timelines = model.timelines |> Dict.values |> List.filter
      (\timeline -> List.member tsId timeline.tsIds)
  in
  case timelines of
    [timeline] -> Just timeline.id
    [] -> logError "findTimelineOfTimespan"
      ("timespan " ++ String.fromInt tsId ++ " not in any timeline") Nothing
    _ -> logError "findTimelineOfTimespan"
      ("timespan " ++ String.fromInt tsId ++ " in more than one timeline") Nothing


-- Zooming

zoom : Model -> ZoomOp -> ( Model, Cmd Msg )
zoom model op =
  let
    delta = case op of
      In -> -1
      Out -> 1
    newModel = { model | zoom = model.zoom + delta }
  in
  ( newModel
  , Cmd.batch
    [ Dom.getViewportOf "tl-scrollcontainer"
      |> Task.andThen (\viewport -> viewport |> adjustViewport op)
      |> Task.attempt (\_ -> NoOp)
    , encode newModel |> store
    ]
  )


adjustViewport : ZoomOp -> Dom.Viewport -> Task Dom.Error ()
adjustViewport op viewport =
  let
    _ = log "tl-scrollcontainer viewport" viewport
    v = viewport.viewport
    w = v.width - 150   -- 150 is width of sticky timeline headers
    (x, y) = case op of
      In ->  (v.x * 2 + w / 2, v.y)
      Out -> (v.x / 2 - w / 4, v.y)
  in
    Dom.setViewportOf "tl-scrollcontainer" x y


-- Settings dialog

updateSetting : Model -> Field -> String -> Model
updateSetting model field value =
  let
    sb = model.settingsBuffer
  in
  case field of
    BeginYear -> { model | settingsBuffer = { sb | beginYear = value } }
    EndYear -> { model | settingsBuffer = { sb | endYear = value } }


closeSettings : Model -> Model
closeSettings model =
  let
    settings_ =
      Maybe.map2
        (\begin end -> Settings begin end)
        (model.settingsBuffer.beginYear |> String.toInt)
        (model.settingsBuffer.endYear |> String.toInt)
  in
  { model
    | settings =
        case settings_ of
          Just settings -> settings
          Nothing -> logError "closeSettings" "Invalid user input" model.settings
    , isSettingsOpen = False
  }


--

reset : Model -> Model
reset model =
  { model
  | selection = NoSelection
  , editState = NoEdit
  }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    NoDrag -> mouseDownSub
    Engaged _ _ _ -> dragSub
    DragTimespan _ _ _ -> dragSub
    DrawRect _ _ _ -> dragSub


mouseDownSub : Sub Msg
mouseDownSub =
  E.onMouseDown <| D.oneOf
    [ D.map3 MouseDownItem
        ( D.map2 Point
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        )
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
    , D.succeed MouseDown
    ]


dragSub : Sub Msg
dragSub =
  Sub.batch
    [ E.onMouseMove <| D.map MouseMove
        ( D.map2 Point
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        )
    , E.onMouseUp (D.succeed MouseUp)
    ]


strToIntDecoder : String -> D.Decoder Int
strToIntDecoder str =
  case String.toInt str of
    Just int -> D.succeed int
    Nothing -> D.fail <| "\"" ++ str ++ "\" is an invalid Timespan ID"



-- VIEW


view : Model -> Html Msg
view model =
  div
    (appStyle model)
    [ editable model TitleEdit titleTextStyle model.title
    , div
        ( [ id "tl-scrollcontainer" ]
          ++ contentStyle
        )
        [ div
            blinderStyle
            []
        , div
            timelineHeadersStyle
            ( Dict.values model.timelines |>
                List.map (\timeline -> viewTimelineHeader model timeline)
            )
        , div
            []
            [ viewTimeScale model
            , div
                [ id "tl-timelines" ]
                ( Dict.values model.timelines |>
                    List.map (\timeline -> viewTimeline model timeline)
                )
            ]
        ]
    , viewToolbar model
    , viewRectangle model
    , viewSettings model
    ]


viewTimeScale : Model -> Html Msg
viewTimeScale model =
  let
    width_ = timelineWidth model |> String.fromInt
  in
  svg
    ( [ width width_
      , height "30"
      , viewBox ("0 0 " ++ width_ ++ " 30")
      ]
      ++ timeScaleStyle
    )
    (List.range model.settings.beginYear model.settings.endYear |> List.map
      (\year ->
        let
          showYear = modBy (yearStep model) year == 0
          x_ = (year - model.settings.beginYear) * pixelPerYear model
          x1_ = x_ |> String.fromInt
          y1_ = if showYear then "0" else "25"
          x2_ = x1_
          y2_ = "30"
          tx = (x_ + 3) |> String.fromInt
        in
        [ line [ x1 x1_, y1 y1_, x2 x2_, y2 y2_, stroke "gray" ] []] ++
        if showYear then
          [ text_ [ x tx, y "12", fill "gray" ] [ text (String.fromInt year) ]]
        else
          []
      )
      |> List.foldr (++) []
    )


timelineWidth : Model -> Int
timelineWidth model =
  (model.settings.endYear - model.settings.beginYear + 1) * pixelPerYear model


viewTimelineHeader : Model -> Timeline -> Html Msg
viewTimelineHeader model timeline =
  let
    selTarget = TimelineSelection timeline.id
    editTarget = TimelineEdit timeline.id
  in
  div
    ( [ onClick (Select selTarget) ]
      ++ timelineHeaderStyle timeline
      ++ selectionBorderStyle model timeline.id
    )
    [ editable model editTarget timelineHeaderTextStyle timeline.title ]


viewTimeline : Model -> Timeline -> Html Msg
viewTimeline model timeline =
  div
    ( [ class "tl-timeline"
      , attribute "data-id" (String.fromInt timeline.id)
      ]
      ++ timelineStyle timeline
    )
    (List.map
      (\tsId ->
        case Dict.get tsId model.timespans of
          Just timespan -> viewTimespan model timespan timeline.color
          Nothing -> illegalTimespanId "viewTimeline" tsId text ""
      )
      timeline.tsIds
    )


viewTimespan : Model -> Timespan -> Hue -> Html Msg
viewTimespan model timespan hue =
  let
    selTarget = TimespanSelection timespan.id
    editTarget = TimespanEdit timespan.id
  in
  div -- don't appy opacity to the selection border -> 2 nested divs
    (timespanBorderStyle model timespan)
    [ div
      ( [ onClick (Select selTarget)
        , class "tl-timespan"
        , attribute "data-id" (String.fromInt timespan.id)
        ]
        ++ timespanStyle model hue
      )
      [ editable model editTarget defaultTextStyle timespan.title
      , viewResizer timespan.id "left"
      , viewResizer timespan.id "right"
      ]
    ]


viewResizer : Id -> String -> Html Msg
viewResizer id pos =
  div
    ( [ class ("tl-resizer-" ++ pos)
      , attribute "data-id" (String.fromInt id)
      ]
      ++ resizerStyle pos
    )
    []


viewToolbar : Model -> Html Msg
viewToolbar model =
  let
    deleteDisabled =
      case model.selection of
        NoSelection -> True
        _ -> False
    settingsDisabled = model.isSettingsOpen
    zoomInDisabled = model.zoom == 0
    zoomOutDisabled = model.zoom == Array.length conf.zoomLevels - 1
  in
  div
    toolbarStyle
    [ button
        ( [ onClick (Zoom In)
          , disabled zoomInDisabled
          ]
          ++ toolbarButtonStyle
        )
        [ text "+"]
    , button
        ( [ onClick (Zoom Out)
          , disabled zoomOutDisabled
          ]
          ++ toolbarButtonStyle
        )
        [ text "-"]
    , button
        ( [ onClick AddTimeline ]
          ++ toolbarButtonStyle
        )
        [ text "Add Timeline"]
    , button
        ( [ onClick Delete
          , stopPropagationOnMousedown -- avoid disabling button before "click" can occur
          , disabled deleteDisabled
          ]
          ++ toolbarButtonStyle
        )
        [ text "Delete"]
    , div spacerStyle []
    , button
        ( [ onClick OpenSettings
          --, stopPropagationOnMousedown -- TODO?
          , disabled settingsDisabled
          ]
          ++ toolbarButtonStyle
        )
        [ text "Settings"]
    ]


viewRectangle : Model -> Html Msg
viewRectangle model =
  case model.dragState of
    DrawRect _ p size ->
      div
        (rectangleStyle p size)
        []
    _ -> text ""


viewSettings : Model -> Html Msg
viewSettings model =
  if model.isSettingsOpen then
    div
      settingsStyle
      [ div
          []
          [ text "Timeline" ]
      , div
          settingsContentStyle
          [ div
              []
              [ text "Begin"]
          , input
              [ value model.settingsBuffer.beginYear
              , onInput (EditSetting BeginYear)
              ]
              []
          , div
              []
              [ text "End"]
          , input
              [ value model.settingsBuffer.endYear
              , onInput (EditSetting EndYear)
              ]
              []
          ]
      , button
          ( [ onClick CloseSettings ]
            ++ settingsButtonStyle
          )
          [ text "x" ]
      ]
  else
    text ""


editable : Model -> EditTarget -> List (Attribute Msg) -> String -> Html Msg
editable model target textStyle title =
  if target /= model.editState then
    div
      ( [ onClick (EditStart target)
        , style "margin-bottom" "6px" -- compensate higher input field height -- TODO
        ]
        ++ textStyle
      )
      [ text title ]
  else
    input
      ( [ value title
        , onInput Edit
        , onEnter EditEnd
        , stopPropagationOnMousedown
        ]
        ++ editStyle
        ++ textStyle
      )
      []


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        D.succeed msg
      else
        D.fail "not ENTER"
  in
    on "keydown" (keyCode |> D.andThen isEnter)


stopPropagationOnMousedown : Attribute Msg
stopPropagationOnMousedown =
  stopPropagationOn "mousedown" <| D.succeed (NoOp, True)



-- DEBUG


illegalTimespanId : String -> Int -> a -> a
illegalTimespanId func id val =
  illegalId func "Timespan" id val


illegalTimelineId : String -> Int -> a -> a
illegalTimelineId func id val =
  illegalId func "Timeline" id val


illegalId : String -> String -> Int -> a -> a
illegalId func item id val =
  logError func (String.fromInt id ++ " is an illegal " ++ item ++ " ID") val
