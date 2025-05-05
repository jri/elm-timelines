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
  { title : String
  , color : Color
  , tsIds : List Int
  }

type alias Timespans = Dict Int Timespan

type alias Timespan =
  { id : Int
  , title : String
  , begin : Int
  , end : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

type alias Color = Int

type alias Id = Int

type DragState
  = Moving Id Point -- object Id, last point
  | None

init : () -> (Model, Cmd Msg)
init _ =
  ( Model
    "Terry's life"
    [ Timeline "Living places" 120 [1, 2] -- green
    , Timeline "Girlfriends" 0 [3, 4, 5] -- red
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


type Msg
  = MouseDown Int Int String String -- x y class id
  | MouseMove Int Int
  | MouseUp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  {--
  let
    _ = log "UPDATE" msg
  in
  --}
  case msg of
    MouseDown x y class idStr ->
      ( { model
          | dragState = updateDragState class idStr x y
        }
      , Cmd.none
      )
    MouseMove x y ->
      ( let
          tsId = dragId model
          delta = x - lastX model
        in
        { model
          | timespans = updateTimespan model tsId delta
          , dragState = Moving tsId <| Point x y
        }
      , Cmd.none
      )
    MouseUp ->
      ( { model | dragState = None }
      , Cmd.none
      )

updateDragState : String -> String -> Int -> Int -> DragState
updateDragState class idStr x y =
  if class == "tl-timespan" then
    case String.toInt idStr of
      Just id -> Moving id (Point x y)
      Nothing -> illegalTimespanIdStr idStr None
  else
    None

updateTimespan : Model -> Id -> Int -> Timespans
updateTimespan model id delta =
  Dict.update
    id
    (\ts -> case ts of
      Just timespan -> Just
        { timespan
          | begin = timespan.begin + delta
          , end = timespan.end + delta
        }
      Nothing -> illegalTimespanId id Nothing
    )
    model.timespans

dragId : Model -> Id
dragId model =
  case model.dragState of
    Moving id _ -> id
    None -> log "### ERROR: handling MouseMove message while not in MOVING state" 0

lastX : Model -> Int
lastX model =
  case model.dragState of
    Moving _ point -> point.x
    None -> log "### ERROR: handling MouseMove message while not in MOVING state" 0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dragState of
    None ->
      E.onMouseDown <| D.map4 MouseDown
        ( D.field "clientX" D.int )
        ( D.field "clientY" D.int )
        ( D.at ["target", "className"] D.string )
        ( D.at ["target", "dataset", "id"] D.string )
    Moving _ _ ->
      Sub.batch
        [ E.onMouseMove <| D.map2 MouseMove
          ( D.field "clientX" D.int )
          ( D.field "clientY" D.int )
        , E.onMouseUp (D.succeed MouseUp)
        ]



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "font-family" "sans-serif"
    , style "margin" "20px"
    ]
    [ h1 [] [ text model.title ]
    , div [] ( List.map ( \timeline -> viewTimeline timeline model ) model.timelines )
    ]

viewTimeline : Timeline -> Model -> Html Msg
viewTimeline timeline model =
  div
    [ style "background-color" (hsl timeline.color "95%")
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
            Nothing -> illegalTimespanId tsId text ""
        )
        timeline.tsIds
      )
    ]

viewTimespan : Timespan -> Color -> Html Msg
viewTimespan timespan color =
  div
    [ class "tl-timespan"
    , attribute "data-id" <| String.fromInt timespan.id
    , style "position" "absolute"
    , style "top" "0"
    , style "left" <| String.fromInt timespan.begin ++ "px"
    , style "width" <| String.fromInt (timespan.end - timespan.begin) ++ "px"
    , style "height" "100%"
    , style "padding" "5px"
    , style "box-sizing" "border-box"
    , style "background-color" (hsl color "90%")
    ]
    [ text timespan.title ]

illegalTimespanId : Int -> a -> a
illegalTimespanId tsId val =
  log ("### ERROR: " ++ String.fromInt tsId ++ " is an illegal Timespan ID") val

illegalTimespanIdStr : String -> a -> a
illegalTimespanIdStr tsIdStr val =
  log ("### ERROR: " ++ tsIdStr ++ " is an illegal Timespan ID string") val

hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"
