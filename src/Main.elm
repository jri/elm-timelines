port module Main exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html, h1, button, div, span, text)
import Html.Attributes exposing (class, attribute, style)
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
  = MouseDown Point Class Id
  | MouseMove Point
  | MouseUp


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
      , (3, Timespan 3 "Barbara" 150 315)
      , (4, Timespan 4 "Caroline" 320 350)
      , (5, Timespan 5 "Marina" 500 600)
      ]
    )
    None
    100 -- TODO: calculate

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
  {--
  let
    _ = log "UPDATE" msg
  in
  --}
  case msg of
    MouseDown p class id -> ( dragStart model class id p, Cmd.none )
    MouseMove p          -> ( drag model p,               Cmd.none )
    MouseUp ->
      let
        newModel = dragStop model
      in
      ( newModel, encode newModel |> store )


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
      logError "Reveived MouseMove when DragState is None" "update" model


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
      E.onMouseDown <| D.map3 MouseDown
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
  [ E.onMouseMove <| D.map MouseMove
    ( D.map2 Point
      ( D.field "clientX" D.int )
      ( D.field "clientY" D.int )
    )
  , E.onMouseUp (D.succeed MouseUp)
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
    , div []
      ( Dict.values model.timelines |>
          List.map ( \timeline -> viewTimeline timeline model )
      )
    , viewRectangle model
    ]


viewTimeline : Timeline -> Model -> Html Msg
viewTimeline timeline model =
  div
    [ class "tl-timeline"
    , attribute "data-id" (String.fromInt timeline.id)
    , style "background-color" (hsl timeline.color "95%")
    , style "height" "60px"
    , style "margin-bottom" "5px"
    , style "cursor" "crosshair"
    ]
    [ div
      [ style "vertical-align" "top"
      , style "display" "inline-block"
      , style "width" "150px"
      , style "margin" "5px"
      ]
      [ text timeline.title ]
    , div
      [ style "position" "relative"
      , style "display" "inline-block"
      , style "height" "100%"
      ]
      ( List.map
        ( \tsId ->
          case Dict.get tsId model.timespans of
            Just timespan -> viewTimespan timespan timeline.color model.dragState
            Nothing -> illegalTimespanId tsId "viewTimeline" text ""
        )
        timeline.tsIds
      )
    ]


viewTimespan : Timespan -> Hue -> DragState -> Html Msg
viewTimespan timespan hue dragState =
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
    [ text timespan.title
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



-- JSON ENCODE/DECODE


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
decoder = D.map5 Model
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
