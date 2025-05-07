module Main exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html, h1, button, div, span, text)
import Html.Attributes exposing (class, attribute, style)
import Dict exposing (Dict)
import Json.Decode as D
import Debug exposing (log)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model =
  { title : String
  , timelines : List Timeline
  , timespans: Timespans
  , dragState : DragState
  }

type alias Timeline = 
  { id : Id
  , title : String
  , color : Color
  , tsIds : List Int
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

type Msg
  = MouseDown Int Int String Int -- x y class id
  | MouseMove Int Int
  | MouseUp

type alias Point =
  { x : Int
  , y : Int
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Color = Int -- hue

type alias Id = Int

init : () -> (Model, Cmd Msg)
init _ =
  ( Model
    "Terry's life"
    [ Timeline 6 "Living places" 120 [1, 2] -- green
    , Timeline 7 "Girlfriends" 0 [3, 4, 5] -- red
    ]
    ( Dict.fromList
      [ (1, Timespan 1 "Park Avenue" 200 350)
      , (2, Timespan 2 "Lake Street" 400 700)
      , (3, Timespan 3 "Barbara" 150 315)
      , (4, Timespan 4 "Caroline" 320 350)
      , (5, Timespan 5 "Marina" 500 600)
      ]
    )
    None
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
    MouseDown x y class id ->
      ( { model | dragState = updateDragState class id x y }
      , Cmd.none
      )
    MouseMove x y ->
      ( case model.dragState of
          DragTimespan id mode lastPoint ->
            let
              delta = x - lastPoint.x
            in
            { model
              | timespans = updateTimespan model id delta mode
              , dragState = DragTimespan id mode (Point x y)
            }
          DrawRect id startPoint size ->
            let
              w = x - startPoint.x
              h = y - startPoint.y
            in
            { model | dragState = DrawRect id startPoint (Size w h) }
          None ->
            logError "Reveived MouseMove when DragState is None" "update" model
      , Cmd.none
      )
    MouseUp ->
      ( { model | dragState = None }
      , Cmd.none
      )

updateDragState : String -> Int -> Int -> Int -> DragState
updateDragState class id x y =
  let
    p = Point x y
  in
  case class of
    "tl-timespan"      -> DragTimespan id MoveTimespan p
    "tl-resizer-left"  -> DragTimespan id MoveBegin p
    "tl-resizer-right" -> DragTimespan id MoveEnd p
    "tl-timeline" -> DrawRect id p (Size 0 0)
    _ -> None

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    toInt : String -> D.Decoder Int
    toInt str =
      case String.toInt str of
        Just int -> D.succeed int
        Nothing -> D.fail <| "\"" ++ str ++ "\" is an invalid Timespan ID"
    drag : Sub Msg
    drag = Sub.batch
      [ E.onMouseMove <| D.map2 MouseMove
        ( D.field "clientX" D.int )
        ( D.field "clientY" D.int )
      , E.onMouseUp (D.succeed MouseUp)
      ]
  in
  case model.dragState of
    None ->
      E.onMouseDown <| D.map4 MouseDown
        ( D.field "clientX" D.int )
        ( D.field "clientY" D.int )
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string |> D.andThen toInt )
    DragTimespan _ _ _ -> drag
    DrawRect _ _ _ -> drag



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "font-family" "sans-serif"
    , style "margin" "20px"
    ]
    [ h1 [] [ text model.title ]
    , div [] ( List.map ( \timeline -> viewTimeline timeline model ) model.timelines )
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
    ]
    [ div
      [ style "vertical-align" "top"
      , style "display" "inline-block"
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
            Just timespan -> viewTimespan timespan timeline.color
            Nothing -> illegalTimespanId tsId "viewTimeline" text ""
        )
        timeline.tsIds
      )
    ]

viewTimespan : Timespan -> Color -> Html Msg
viewTimespan timespan color =
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
    , style "background-color" (hsl color "90%")
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
        ]
        []
    _ -> text ""

hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"

illegalTimespanId : Int -> String -> a -> a
illegalTimespanId tsId func val =
  logError (String.fromInt tsId ++ " is an illegal Timespan ID") func val

logError : String -> String -> a -> a
logError text func val =
  log ("ERROR in " ++ func ++ ": " ++ text) val
