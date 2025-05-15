port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html, Attribute, h1, div, span, text, button, input)
import Html.Attributes exposing (class, id, attribute, style, value, disabled)
import Html.Events exposing (onClick, on, keyCode, targetValue)
import Svg exposing (svg, line, text_) -- "text_" is an element, "text" is a node
import Svg.Attributes exposing (viewBox, width, height, x, y, x1, y1, x2, y2, stroke, fill)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Task
import Debug exposing (log)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- PORTS


port store : E.Value -> Cmd msg



-- MODEL


type alias Model =
  { title : String
  , timelines : Timelines
  , timespans : Timespans
  , dragState : DragState -- transient
  , editState : Target -- transient
  , selection : Target -- transient
  , nextId : Id
  }


type alias Timelines = Dict Int Timeline


type alias Timeline =
  { id : Id
  , title : String
  , color : Hue
  , tsIds : List Id
  }


type alias Timespans = Dict Int Timespan


type alias Timespan =
  { id : Id
  , title : String
  , begin : Int
  , end : Int
  }


type DragState
  = NoDrag
  | Engaged Point Class Id
  | DragTimespan Id TimespanMode Point -- timespan id, mode, last point
  | DrawRect Id Point Size -- timeline id, start point, width/height


type TimespanMode
  = MoveTimespan
  | MoveBegin
  | MoveEnd


type Target
  = TimelineTarget Id
  | TimespanTarget Id
  | NoTarget


type alias Point =
  { x : Int
  , y : Int
  }


type alias Size =
  { width : Int
  , height : Int
  }


type alias Class = String
type alias Id = Int
type alias Hue = Int


type Msg
  = AddTimeline
  | AddTimespan Id Point Size (Result Dom.Error Dom.Element) -- timeline id
  | Select Target
  | EditStart Target
  | EditEnd String
  | MouseDown Point Class Id
  | MouseMove Point
  | MouseUp
  | Delete


conf =
  { beginYear = 1980
  , endYear = 2025
  , pixelPerYear = 64
  , selectionColor = "#007AFF" -- Firefox focus color
  }


defaultModel : Model
defaultModel =
  Model
    "Terry's life"
    ( Dict.fromList
      [ (6, Timeline 6 "Living places" 120 [1, 2]) -- green
      , (7, Timeline 7 "Girlfriends" 0 [3, 4, 5]) -- red
      ]
    )
    ( Dict.fromList
      [ (1, Timespan 1 "Park Avenue" 200 350)
      , (2, Timespan 2 "Lake Street" 400 700)
      , (3, Timespan 3 "Barbara" 128 315)
      , (4, Timespan 4 "Caroline" 330 360)
      , (5, Timespan 5 "Marina" 500 600)
      ]
    )
    NoDrag
    NoTarget
    NoTarget
    100


init : E.Value -> ( Model, Cmd Msg )
init flags =
  ( case D.decodeValue decoder flags of
      Ok model ->
        log "Reading from localStorage" model
      Err e ->
        let
          _ = log "Reading from localStorage" e
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
    AddTimeline ->
      let
        newModel = addTimeline model
      in
      ( newModel, encode newModel |> store )
    AddTimespan tlId point size result ->
      let
        newModel = addTimespan model tlId point size result
      in
      ( newModel, encode newModel |> store )
    Select target -> ( { model | selection = target }, Cmd.none )
    EditStart target -> ( { model | editState = target }, Cmd.none )
    EditEnd title ->
      let
        newModel_ = updateTitle model title
        newModel = { newModel_ | editState = NoTarget }
      in
      ( newModel, encode newModel |> store )
    MouseDown p class id -> ( { model | dragState = Engaged p class id }, Cmd.none )
    MouseMove p -> ( mouseMove model p, Cmd.none )
    MouseUp -> mouseUp model
    Delete ->
      let
        newModel = delete model
      in
      ( newModel, encode newModel |> store )


addTimeline : Model -> Model
addTimeline model =
  let
    id = model.nextId
  in
    { model
      | timelines = model.timelines |> Dict.insert id
        ( Timeline id "New Timeline" 0 [] ) -- TODO: color
      , editState = TimelineTarget id
      , nextId = id + 1
    }


addTimespan : Model -> Id -> Point -> Size -> Result Dom.Error Dom.Element -> Model
addTimespan model tlId point size result =
  case result of
    Ok element ->
      let
        tsId = model.nextId
        begin = point.x - round element.element.x
        end = begin + size.width
      in
      { model
        | timelines = updateTimeline model tlId tsId
        , timespans = insertNewTimespan model tsId begin end
        , editState = TimespanTarget tsId
        , nextId = model.nextId + 1
      }
    Err (Dom.NotFound e) -> logError "addTimespan" ("Dom.NotFound \"" ++ e ++ "\"") model


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
      DrawRect tlId point size ->
        Task.attempt (AddTimespan tlId point size) (Dom.getElement "tl-timelines")
      _ -> Cmd.none
  )


updateTimespan : Model -> Id -> Int -> TimespanMode -> Timespans
updateTimespan model id delta mode =
  Dict.update
    id
    (\ts -> case ts of
      Just timespan -> Just <|
        case mode of
          MoveTimespan ->
            { timespan
              | begin = timespan.begin + delta
              , end = timespan.end + delta
            }
          MoveBegin ->
            { timespan | begin = timespan.begin + delta }
          MoveEnd ->
            { timespan | end = timespan.end + delta }
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


insertNewTimespan : Model -> Id -> Int -> Int -> Timespans
insertNewTimespan model tsId begin end =
  model.timespans |> Dict.insert tsId
    ( Timespan tsId "New Timespan" begin end )


updateTitle : Model -> String -> Model
updateTitle model title =
  case model.editState of
    TimelineTarget id ->
      { model | timelines = model.timelines |>
        Dict.update id
          (\tl -> case tl of
            Just timeline -> Just { timeline | title = title }
            Nothing -> illegalTimelineId "updateTitle" id Nothing
          )
      }
    TimespanTarget id ->
      { model | timespans = model.timespans |>
        Dict.update id
          (\ts -> case ts of
            Just timespan -> Just { timespan | title = title }
            Nothing -> illegalTimespanId "updateTitle" id Nothing
          )
      }
    NoTarget -> logError "updateTitle" "called when editState is NoTarget" model


delete : Model -> Model
delete model =
  case model.selection of
    TimelineTarget id ->
      case model.timelines |> Dict.get id of
        Just timeline ->
          { model
            | timelines = model.timelines |> Dict.remove id
            , timespans = model.timespans |> Dict.filter
                (\tsId timespan -> not (List.member tsId timeline.tsIds))
            , selection = NoTarget
          }
        Nothing -> illegalTimelineId "delete" id model
    TimespanTarget id ->
      case model.timespans |> Dict.get id of
        Just timespan ->
          { model
            | timelines = removeFromTimeline model id
            , timespans = model.timespans |> Dict.remove id
            , selection = NoTarget
          }
        Nothing -> illegalTimespanId "delete" id model
    NoTarget -> logError "delete" "called when \"selection\" state is NoTarget" model


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
  E.onMouseDown <| D.map3 MouseDown
    ( D.map2 Point
      ( D.field "clientX" D.int )
      ( D.field "clientY" D.int )
    )
    ( D.at ["target", "className"] D.string )
    ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )


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
  let
    userSelect =
      case model.dragState of
        NoDrag -> "auto"
        _ -> "none"
  in
  div
    [ style "font-family" "sans-serif"
    , style "font-size" "16px"
    , style "margin" "20px"
    , style "user-select" userSelect
    ]
    [ h1 [] [ text model.title ]
    , div
      [ style "display" "flex" ]
      [ div
        [ style "margin-top" "30px" ]
        ( Dict.values model.timelines |>
            List.map ( \timeline -> viewTimelineHeader model timeline )
        )
      , div
        [ style "overflow" "auto" ]
        [ viewTimeScale
        , div
          [ id "tl-timelines" ]
          ( Dict.values model.timelines |>
              List.map ( \timeline -> viewTimeline model timeline )
          )
        ]
      ]
    , viewToolbar model
    , viewRectangle model
    ]


viewTimeScale : Html Msg
viewTimeScale =
  svg
    [ width "2000"
    , height "30"
    , viewBox "0 0 2000 30"
    , style "font-size" "14px"
    , style "margin-bottom" "1px"
    -- , style "background-color" "beige"
    ]
    ( List.range conf.beginYear conf.endYear |> List.map
      (\year ->
        let
          x_ = (year - conf.beginYear) * conf.pixelPerYear
          x1_ = x_ |> String.fromInt
          y1_ = "0"
          x2_ = x1_
          y2_ = "40"
          tx = (x_ + 3) |> String.fromInt
        in
        [ line [ x1 x1_, y1 y1_, x2 x2_, y2 y2_, stroke "gray" ] []
        , text_ [ x tx, y "12", fill "gray" ] [ text (String.fromInt year) ]
        ]
      )
      |> List.foldr (++) []
    )


viewToolbar : Model -> Html Msg
viewToolbar model =
  let
    disabled_ =
      case model.selection of
        NoTarget -> True
        _ -> False
  in
  div
    [ style "margin-top" "26px" ]
    [ button
      [ onClick AddTimeline ]
      [ text "Add Timeline"]
    , button
      [ onClick Delete
      , disabled disabled_
      , style "margin-left" "26px"
      ]
      [ text "Delete"]
    ]


viewTimelineHeader : Model -> Timeline -> Html Msg
viewTimelineHeader model timeline =
  let
    target = TimelineTarget timeline.id
  in
  div
    [ onClick (Select target)
    , selectionBorder model timeline.id
    , style "background-color" (hsl timeline.color "95%")
    , style "font-size" "14px"
    , style "font-weight" "bold"
    , style "width" "150px"
    , style "height" "60px"
    , style "margin-top" "5px"
    , style "padding" "5px 6px"
    , style "box-sizing" "border-box"
    ]
    [ inlineEdit model target timeline.title ]


viewTimeline : Model -> Timeline -> Html Msg
viewTimeline model timeline =
  div
    [ class "tl-timeline"
    , attribute "data-id" (String.fromInt timeline.id)
    , style "position" "relative"
    , style "height" "60px"
    , style "margin-bottom" "5px"
    , style "background-color" (hsl timeline.color "95%")
    , style "cursor" "crosshair"
    ]
    ( List.map
      ( \tsId ->
        case Dict.get tsId model.timespans of
          Just timespan -> viewTimespan model timespan timeline.color model.dragState
          Nothing -> illegalTimespanId "viewTimeline" tsId text ""
      )
      timeline.tsIds
    )


viewTimespan : Model -> Timespan -> Hue -> DragState -> Html Msg
viewTimespan model timespan hue dragState =
  let
    target = TimespanTarget timespan.id
    cursor =
      case dragState of
        DragTimespan _ _ _ -> "grabbing"
        _ -> "grab"
  in
  div -- don't appy opacity to the selection border -> 2 nested divs
    [ selectionBorder model timespan.id
    , style "position" "absolute"
    , style "top" "0"
    , style "left" (String.fromInt timespan.begin ++ "px")
    , style "width" (String.fromInt (timespan.end - timespan.begin) ++ "px")
    , style "height" "100%"
    , style "box-sizing" "border-box"
    ]
    [ div
      [ onClick (Select target)
      , class "tl-timespan"
      , attribute "data-id" (String.fromInt timespan.id)
      , style "height" "100%"
      , style "padding" "5px 6px"
      , style "box-sizing" "border-box"
      , style "background-color" (hsl hue "60%")
      , style "opacity" "0.5"
      , style "cursor" cursor
      ]
      [ inlineEdit model target timespan.title
      , viewResizer timespan.id "left"
      , viewResizer timespan.id "right"
      ]
    ]


viewResizer : Id -> String -> Html Msg
viewResizer id pos =
  div
    [ class ("tl-resizer-" ++ pos)
    , attribute "data-id" (String.fromInt id)
    , style "position" "absolute"
    , style "top" "0"
    , style pos "-5px"
    , style "width" "10px"
    , style "height" "100%"
    , style "z-index" "1" -- keeps cursor when hovering other timespan
    , style "cursor" "col-resize"
    -- , style "background-color" "hsl(0, 0%, 50%)" -- for debugging
    -- , style "opacity" "0.2"
    ]
    []


viewRectangle : Model -> Html Msg
viewRectangle model =
  case model.dragState of
    DrawRect _ p size ->
      div
        [ style "position" "absolute"
        , style "top" (String.fromInt p.y ++ "px")
        , style "left" (String.fromInt p.x ++ "px")
        , style "width" (String.fromInt size.width ++ "px")
        , style "height" (String.fromInt size.height ++ "px")
        , style "border" "1px dashed gray"
        , style "cursor" "crosshair"
        ]
        []
    _ -> text ""


inlineEdit : Model -> Target -> String -> Html Msg
inlineEdit model target title =
  let
    v = case target of
      TimelineTarget id -> { id = Just id, fs = "14px", fw = "bold" }
      TimespanTarget id -> { id = Just id, fs = "16px", fw = "normal" }
      NoTarget -> { id = Nothing, fs = "", fw = "" }
  in
  case v.id of
    Just id ->
      if isActive model .editState id then
        input
          [ value title
          , style "position" "relative"
          , style "top" "-3px"
          , style "left" "-4px"
          , style "width" "130px"
          , style "font-family" "sans-serif" -- Default (on Mac) is "-apple-system"
          , style "font-size" v.fs
          , style "font-weight" v.fw
          , onEnter EditEnd
          ]
          []
      else
        div
          [ onClick (EditStart target) ]
          [ text title ]
    Nothing -> logError "inlineEdit" "called when target is NoTarget" (text "")


onEnter : (String -> Msg) -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        targetValue
      else
        D.fail "not ENTER"
    readValue value =
      D.succeed (msg value)
  in
    on "keydown" (keyCode |> D.andThen isEnter |> D.andThen readValue)


selectionBorder : Model -> Id -> Attribute Msg
selectionBorder model id =
  style "border"
    ( "2px solid " ++
      if isActive model .selection id then
        conf.selectionColor
      else
        "transparent"
    )


isActive : Model -> (Model -> Target) -> Id -> Bool
isActive model targetFunc id =
  case targetFunc model of
    TimelineTarget id_ -> id_ == id
    TimespanTarget id_ -> id_ == id
    NoTarget -> False



-- ENCODE/DECODE JS VALUES


encode : Model -> E.Value
encode model =
  E.object
    [ ("title", E.string model.title)
    , ("timelines", E.dict
        String.fromInt
        ( \timeline -> E.object
          [ ("id",    E.int        timeline.id)
          , ("title", E.string     timeline.title)
          , ("color", E.int        timeline.color)
          , ("tsIds", E.list E.int timeline.tsIds)
          ]
        )
        model.timelines
      )
    , ("timespans", E.dict
        String.fromInt
        ( \timespan -> E.object
          [ ("id",    E.int    timespan.id)
          , ("title", E.string timespan.title)
          , ("begin", E.int    timespan.begin)
          , ("end",   E.int    timespan.end)
          ]
        )
        model.timespans
      )
    , ("nextId", E.int model.nextId)
    ]


decoder : D.Decoder Model
decoder = D.map7 Model
  (D.field "title" D.string)
  (D.field "timelines"
    (D.dict
      (D.map4 Timeline
        (D.field "id"    D.int)
        (D.field "title" D.string)
        (D.field "color" D.int)
        (D.field "tsIds" (D.list D.int))
      ) |> D.andThen strToIntDictDecoder
    )
  )
  (D.field "timespans"
    (D.dict
      (D.map4 Timespan
        (D.field "id"    D.int)
        (D.field "title" D.string)
        (D.field "begin" D.int)
        (D.field "end"   D.int)
      ) |> D.andThen strToIntDictDecoder
    )
  )
  (D.succeed NoDrag)
  (D.succeed NoTarget)
  (D.succeed NoTarget)
  (D.field "nextId" D.int)


strToIntDictDecoder : Dict String v -> D.Decoder (Dict Int v)
strToIntDictDecoder strDict =
  case strToIntDict strDict of
    Just dict -> D.succeed dict
    Nothing -> D.fail "Transformation Dict String -> Int failed"


strToIntDict : Dict String v -> Maybe (Dict Int v)
strToIntDict strDict =
  strDict |> Dict.foldl
    ( \k v b ->
      case b of
        Just b_ ->
          case String.toInt k of
            Just i -> Just (Dict.insert i v b_)
            Nothing -> Nothing
        Nothing -> Nothing
    )
    (Just Dict.empty)



-- HELPER


hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"


illegalTimespanId : String -> Int -> a -> a
illegalTimespanId func id val =
  logError func (String.fromInt id ++ " is an illegal Timespan ID") val


illegalTimelineId : String -> Int -> a -> a
illegalTimelineId func id val =
  logError func (String.fromInt id ++ " is an illegal Timeline ID") val


logError : String -> String -> a -> a
logError func text val =
  log ("ERROR in " ++ func ++ ": " ++ text) val
