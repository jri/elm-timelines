port module Main exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html, Attribute, h1, div, span, text, button, input)
import Html.Attributes exposing (class, attribute, style, value)
import Html.Events exposing (onClick, on, keyCode)
import Svg exposing (svg, line, text_) -- "text_" is an element, "text" is a node
import Svg.Attributes exposing (viewBox, width, height, x, y, x1, y1, x2, y2, stroke, fill)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
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
  , editMode : EditMode   -- transient
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
  = DragTimespan Id TimespanMode Point -- timespan id, mode, last point
  | DrawRect Id Point Size -- timeline id, start point, width/height
  | None


type TimespanMode
  = MoveTimespan
  | MoveBegin
  | MoveEnd


type EditMode
  = EditTimeline Id
  | EditTimespan Id
  | EditNone


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
  = DragStart Point Class Id
  | Drag Point
  | DragEnd
  | Edit EditMode
  | EnterKey


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
    None
    EditNone
    100


conf =
  { beginYear = 1980
  , endYear = 2025
  , pixelPerYear = 64
  }


init : E.Value -> ( Model, Cmd Msg )
init flags =
  ( case D.decodeValue decoder flags of
      Ok model ->
        log "Read from localStorage" model
      Err e ->
        let
          _ = log "Could not read model from localStorage" e
        in
          defaultModel
    , Cmd.none
  )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  {--}
  let
    _ = log "UPDATE" msg
  in
  --}
  case msg of
    DragStart p class id -> ( dragStart model class id p, Cmd.none )
    Drag p -> ( drag model p, Cmd.none )
    DragEnd ->
      let
        newModel = dragStop model
      in
      ( newModel, encode newModel |> store )
    Edit editMode -> ( {model | editMode = editMode}, Cmd.none )
    EnterKey -> ( model, Cmd.none ) -- TODO


dragStart : Model -> Class -> Id -> Point -> Model
dragStart model class id p =
  { model | dragState = case class of
    "tl-timespan"      -> DragTimespan id MoveTimespan p
    "tl-resizer-left"  -> DragTimespan id MoveBegin p
    "tl-resizer-right" -> DragTimespan id MoveEnd p
    "tl-timeline" -> DrawRect id p (Size 0 0)
    _ -> None
  }


drag : Model -> Point -> Model
drag model p =
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
      { model | dragState = DrawRect id startPoint (Size w h) }
    None ->
      logError "Received Drag when DragState is None" "update" model


dragStop : Model -> Model
dragStop model =
  case model.dragState of
    DrawRect tlId point size ->
      let
        tsId = model.nextId
        begin = point.x - 150 - 20 - 10 -- width-margin-padding -- TODO
        end = begin + size.width
      in
      { model
        | timelines = updateTimeline model tlId tsId
        , timespans = insertNewTimespan model tsId begin end
        , nextId = model.nextId + 1
        , dragState = None
      }
    _ -> { model | dragState = None }


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
      Nothing -> illegalTimespanId id "updateTimespan" Nothing
    )
    model.timespans


updateTimeline : Model -> Id -> Id -> Timelines
updateTimeline model tlId tsId =
  Dict.update
    tlId
    (\tl -> case tl of
      Just timeline -> Just { timeline | tsIds = tsId :: timeline.tsIds }
      Nothing -> illegalTimelineId tlId "updateTimeline" Nothing
    )
    model.timelines


insertNewTimespan : Model -> Id -> Int -> Int -> Timespans
insertNewTimespan model tsId begin end =
  Dict.insert
    tsId
    ( Timespan tsId "New Timespan" begin end )
    model.timespans



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    None ->
      E.onMouseDown <| D.map3 DragStart
        ( D.map2 Point
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        )
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen strToIntDecoder )
    DragTimespan _ _ _ -> dragSub
    DrawRect _ _ _ -> dragSub


strToIntDecoder : String -> D.Decoder Int
strToIntDecoder str =
  case String.toInt str of
    Just int -> D.succeed int
    Nothing -> D.fail <| "\"" ++ str ++ "\" is an invalid Timespan ID"


dragSub : Sub Msg
dragSub = Sub.batch
  [ E.onMouseMove <| D.map Drag
    ( D.map2 Point
      ( D.field "clientX" D.int )
      ( D.field "clientY" D.int )
    )
  , E.onMouseUp (D.succeed DragEnd)
  ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    userSelect =
      case model.dragState of
        None -> "auto"
        _ -> "none"
  in
  div
    [ style "font-family" "sans-serif"
    , style "margin" "20px"
    , style "user-select" userSelect
    ]
    [ h1 [] [ text model.title ]
    , div
      [ style "display" "flex" ]
      [ div
        [ style "margin-top" "30px" ]
        ( Dict.values model.timelines |>
            List.map ( \timeline -> viewTimelineHeader timeline model )
        )
      , div
        [ style "overflow" "auto" ]
        [ viewTimeScale
        , div []
          ( Dict.values model.timelines |>
              List.map ( \timeline -> viewTimeline timeline model )
          )
        ]
      ]
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


viewTimelineHeader : Timeline -> Model -> Html Msg
viewTimelineHeader timeline model =
  div
    [ style "background-color" (hsl timeline.color "95%")
    , style "width" "150px"
    , style "height" "60px"
    , style "margin-top" "5px"
    , style "padding" "5px"
    , style "box-sizing" "border-box"
    ]
    [ inlineEdit model (EditTimeline timeline.id) timeline.id timeline.title ]


viewTimeline : Timeline -> Model -> Html Msg
viewTimeline timeline model =
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
          Nothing -> illegalTimespanId tsId "viewTimeline" text ""
      )
      timeline.tsIds
    )


viewTimespan : Model -> Timespan -> Hue -> DragState -> Html Msg
viewTimespan model timespan hue dragState =
  let
    cursor =
      case dragState of
        DragTimespan _ _ _ -> "grabbing"
        _ -> "grab"
  in
  div
    [ class "tl-timespan"
    , attribute "data-id" (String.fromInt timespan.id)
    , style "position" "absolute"
    , style "top" "0"
    , style "left" (String.fromInt timespan.begin ++ "px")
    , style "width" (String.fromInt (timespan.end - timespan.begin) ++ "px")
    , style "height" "100%"
    , style "padding" "5px"
    , style "box-sizing" "border-box"
    , style "background-color" (hsl hue "90%")
    , style "cursor" cursor
    ]
    [ inlineEdit model (EditTimespan timespan.id) timespan.id timespan.title
    , viewResizer timespan.id "left"
    , viewResizer timespan.id "right"
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


inlineEdit : Model -> EditMode -> Id -> String -> Html Msg
inlineEdit model editMode id title =
  div
    [ onClick (Edit editMode) ]
    [ if isEditMode model id then
        input
          [ value title
          , style "width" "130px"
          , onEnter EnterKey
          ]
          []
      else
        text title
    ]


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


isEditMode : Model -> Id -> Bool
isEditMode model id =
  case model.editMode of
    EditTimeline id_ -> id_ == id
    EditTimespan id_ -> id_ == id
    EditNone -> False




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
decoder = D.map6 Model
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
  (D.succeed None)
  (D.succeed EditNone)
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


illegalTimespanId : Int -> String -> a -> a
illegalTimespanId id func val =
  logError (String.fromInt id ++ " is an illegal Timespan ID") func val


illegalTimelineId : Int -> String -> a -> a
illegalTimelineId id func val =
  logError (String.fromInt id ++ " is an illegal Timeline ID") func val


logError : String -> String -> a -> a
logError text func val =
  log ("ERROR in " ++ func ++ ": " ++ text) val
