port module Main exposing (..)

import Model exposing (..)
import Style exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html, Attribute, div, text, button, input, a)
import Html.Attributes exposing (class, id, style, attribute, value, disabled, href)
import Html.Events exposing (onClick, onInput, on, stopPropagationOn, keyCode)
import Svg exposing (svg, line, text_) -- "text_" is an element, "text" is a node
import Svg.Attributes exposing (viewBox, width, height, x, y, x1, y1, x2, y2, stroke, fill)
import Dict exposing (Dict)
import Array
import Json.Decode as D
import Json.Encode as E
import Task exposing (Task)
--import Debug exposing (log, toString)



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
    AddTimespan tlId point size viewport ->
      addTimespan model tlId point size viewport |> updateAndStore
    EditStart target -> startEditing target model
    Edit title -> ( updateTitle model title, Cmd.none ) |> updateAndStore
    EditEnd -> ( { model | editState = NoEdit }, Cmd.none )
    MouseDown -> ( reset model, Cmd.none )
    MouseDownItem p class id -> ( mouseDownOnItem model p class id, Cmd.none )
    MouseMove p -> ( mouseMove model p, Cmd.none )
    MouseUp -> mouseUp model
    Delete -> ( delete model, Cmd.none ) |> updateAndStore
    -- Zooming
    Zoom op -> zoom model op
    -- Settings dialog
    OpenSettings -> ( { model | isSettingsOpen = True }, Cmd.none )
    EditSetting field value -> ( updateSetting model field value, Cmd.none )
    CloseSettings -> ( closeSettings model, Cmd.none ) |> updateAndStore
    --
    NoOp -> ( model, Cmd.none )


updateAndStore : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateAndStore ( model, cmd ) =
  ( model
  , Cmd.batch
      [ cmd
      , encode model |> store
      ]
  )


addTimeline : Model -> ( Model, Cmd Msg )
addTimeline model =
  let
    id = model.nextId
    colorIndex = modBy (Array.length timelineColors) (Dict.size model.timelines)
    color = case timelineColors |> Array.get colorIndex of
      Just color_ -> color_
      Nothing -> logError "addTimeline" "can't compute timeline color" 0
  in
  { model
    | timelines = model.timelines |> Dict.insert id
      ( Timeline id "New Timeline" color [] )
    , nextId = id + 1
  }
  |> startEditing (TimelineEdit id)


addTimespan : Model -> Id -> Point -> Size -> Dom.Viewport -> ( Model, Cmd Msg )
addTimespan model tlId point size viewport =
  let
    tsId = model.nextId
    x = point.x + round viewport.viewport.x - 170 -- 170 = timeline header width + app padding
    ( begin, end ) = toModelSpace model x size.width
    timespan = Timespan tsId "New Timespan" begin end 0 0
  in
  { model
    | timelines = updateTimeline model tlId tsId
    , timespans = model.timespans |> Dict.insert tsId timespan
    , selection = TimespanSelection tsId
    , nextId = model.nextId + 1
  }
  |> startEditing (TimespanEdit tsId)


startEditing : EditState -> Model -> ( Model, Cmd Msg )
startEditing target model =
  ( { model | editState = target }
  , focus target
  )


focus : EditState -> Cmd Msg
focus target =
  let
    nodeId =
      case target of
        TimelineEdit id -> "tl-input-" ++ String.fromInt id
        TimespanEdit id -> "tl-input-" ++ String.fromInt id
        TitleEdit -> "tl-title-input"
        NoEdit -> logError "focus" "called with target NoEdit" ""
  in
  Dom.focus nodeId |> Task.attempt
    (\result ->
      case result of
        Ok () -> NoOp
        Err e -> logError "focus" (toString e) NoOp
    )


mouseDownOnItem : Model -> Point -> Class -> Id -> Model
mouseDownOnItem model p class id =
  let
    newModel =
      case class of
        "tl-timespan" -> select model (TimespanSelection id)
        "tl-timeline-header" -> select model (TimelineSelection id)
        "tl-timeline" -> reset model
        _ -> model
  in
  if class == "tl-timeline-header" then
    newModel
  else
    { newModel | dragState = DragEngaged p class id }


mouseMove : Model -> Point -> Model
mouseMove model p =
  case model.dragState of
    DragEngaged p_ class id ->
      let
        dragState = case class of
          "tl-timespan"      -> DragTimespan id MoveTimespan p_
          "tl-resizer-left"  -> DragTimespan id MoveBegin p_
          "tl-resizer-right" -> DragTimespan id MoveEnd p_
          "tl-slider-left"   -> DragTimespan id MoveBeginMargin p_
          "tl-slider-right"  -> DragTimespan id MoveEndMargin p_
          "tl-timeline" -> DrawRect id p_ (Size 0 0)
          _ -> NoDrag -- the error will be logged in performDrag
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
    DragEngaged _ _ _ ->
      logError "performDrag" "Received MouseMove message when dragState is DragEngaged" model
    NoDrag ->
      logError "performDrag" "Received MouseMove message when dragState is NoDrag" model


mouseUp : Model -> ( Model, Cmd Msg )
mouseUp model =
  let
    cmd =
      case model.dragState of
        NoDrag -> logError "mouseUp" "Received MouseUp when dragState is NoDrag" Cmd.none
        DragEngaged _ _ _ -> log "Hint: MouseUp without dragging" Cmd.none
        DragTimespan _ _ _ -> encode model |> store
        DrawRect tlId point size ->
          if size.width >= widthThreshold then
            Dom.getViewportOf "tl-scrollcontainer" |> Task.attempt
              (\result ->
                case result of
                  Ok viewport -> AddTimespan tlId point size viewport
                  Err e -> logError "mouseUp" (toString e) NoOp
              )
          else
            log ("Hint: too small for creating timespan (" ++ String.fromInt size.width
              ++ "px < " ++ String.fromInt widthThreshold ++ "px)")
              Cmd.none
  in
  ( { model | dragState = NoDrag }, cmd )


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
          MoveBeginMargin ->
            { timespan | beginMargin = timespan.beginMargin - delta_ |> max 0 }
          MoveEndMargin ->
            { timespan | endMargin = timespan.endMargin + delta_ |> max 0 }
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

select : Model -> SelectionState -> Model
select model target =
  { model
    | selection = target
    , editState = NoEdit
  }


reset : Model -> Model
reset model =
  { model
    | selection = NoSelection
    , editState = NoEdit
  }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ case model.dragState of
        NoDrag -> mouseDownSub
        DragEngaged _ _ _ -> dragSub
        DragTimespan _ _ _ -> dragSub
        DrawRect _ _ _ -> dragSub
    , if model.selection /= NoSelection && model.editState == NoEdit then
        E.onKeyDown (keyDecoder 8 Delete)
      else
        Sub.none
    ]


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
    [ editable model TitleEdit "tl-title-input" titleTextStyle model.title
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
                []
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
    id = timeline.id
    editTarget = TimelineEdit id
    inputId = "tl-input-" ++ String.fromInt id
  in
  div
    ( [ class "tl-timeline-header"
      , attribute "data-id" (String.fromInt id)
      ]
      ++ timelineHeaderStyle timeline
      ++ selectionBorderStyle model id
    )
    [ editable model editTarget inputId timelineHeaderTextStyle timeline.title ]


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
    id = timespan.id
    editTarget = TimespanEdit id
    inputId = "tl-input-" ++ String.fromInt id
  in
  div
    ( [ class "tl-timespan"
      , attribute "data-id" (String.fromInt id)
      ]
      ++ timespanStyle model timespan hue
      ++ selectionBorderStyle model id
    )
    ( [ editable model editTarget inputId defaultTextStyle timespan.title
      , div
          (gradientStyle model timespan hue "left")
          []
      , div
          (gradientStyle model timespan hue "right")
          []
      ]
      ++
        if isSelected model id then
          [ viewResizer id "left"
          , viewResizer id "right"
          , viewSlider model timespan "left"
          , viewSlider model timespan "right"
          ]
        else
          []
    )


viewResizer : Id -> String -> Html Msg
viewResizer id pos =
  div
    ( [ class ("tl-resizer-" ++ pos)
      , attribute "data-id" (String.fromInt id)
      ]
      ++ resizerStyle pos
    )
    []


viewSlider : Model -> Timespan -> String -> Html Msg
viewSlider model timespan pos =
  div
    ( [ class ("tl-slider-" ++ pos)
      , attribute "data-id" (String.fromInt timespan.id)
      ]
      ++ sliderStyle model timespan pos
    )
    []


viewToolbar : Model -> Html Msg
viewToolbar model =
  let
    zoomInDisabled = model.zoom == 0
    zoomOutDisabled = model.zoom == Array.length zoomLevels - 1
    deleteDisabled = model.selection == NoSelection
    settingsDisabled = model.isSettingsOpen
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
    , viewFooter
    , button
        ( [ onClick OpenSettings
          , disabled settingsDisabled
          ]
          ++ toolbarButtonStyle
        )
        [ text "Settings"]
    ]


viewFooter : Html Msg
viewFooter =
  div
    footerStyle
    [ text "Elm Timelines 1.0 by "
    , a
        ( [ href "https://github.com/jri" ]
          ++ linkStyle
        )
        [ text "Jörg Richter" ]
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


editable : Model -> EditState -> String -> List (Attribute Msg) -> String -> Html Msg
editable model target inputId textStyle title =
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
      ( [ id inputId
        , value title
        , onInput Edit
        , onEnterOrEsc EditEnd
        , stopPropagationOnMousedown
        ]
        ++ editStyle
        ++ textStyle
      )
      []


onEnterOrEsc : Msg -> Attribute Msg
onEnterOrEsc msg =
  on "keydown"
    ( D.oneOf
        [ keyDecoder 13 msg
        , keyDecoder 27 msg
        ]
    )


stopPropagationOnMousedown : Attribute Msg
stopPropagationOnMousedown =
  stopPropagationOn "mousedown" <| D.succeed (NoOp, True)



-- UTILS


keyDecoder : Int -> Msg -> D.Decoder Msg
keyDecoder key msg =
  let
    isKey code =
      if code == key then
        D.succeed msg
      else
        D.fail "not that key"
  in
    keyCode |> D.andThen isKey



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
