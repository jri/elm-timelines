module Main exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html, h1, button, div, span, text)
import Html.Attributes exposing (class, style)
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
  , timespans: Dict Int Timespan
  , dragState : DragState
  }

type alias Timeline = 
  { title : String
  , color : Color
  , tsIds : List Int
  }

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
  = None
  | Moving Id Point

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
  = MouseDown Int Int String
  | MouseMove Int Int
  | MouseUp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    _ = log "UPDATE" msg
  in
  case msg of
    MouseDown x y class ->
      ( { model | dragState = if class == "tl-timespan" then Moving 0 (Point x y) else None }
      , Cmd.none
      )
    MouseMove x y ->
      ( let
          d = x - startX model.dragState
        in
        model -- { model | timelines.entries = timespan.begin + d } -- TODO
      , Cmd.none
      )
    MouseUp ->
      ( { model | dragState = None }
      , Cmd.none
      )

startX : DragState -> Int
startX dragState =
  case dragState of
    Moving id point -> point.x
    None -> log "### ERROR: handling MouseMove message while not in MOVING state" 0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    _ = log "SUBSCRIPTIONS" 0
  in
  case model.dragState of
    None ->
      E.onMouseDown <| D.map3 MouseDown
        ( D.field "offsetX" D.int )
        ( D.field "offsetY" D.int )
        ( D.at ["target", "className"] D.string )
    Moving _ _ ->
      Sub.batch
        [ E.onMouseMove <| D.map2 MouseMove
          ( D.field "offsetX" D.int )
          ( D.field "offsetY" D.int )
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
          let
            ts = Dict.get tsId model.timespans
          in
          case ts of
            Just timespan -> viewTimespan timespan timeline.color
            Nothing -> illegalTimespanId tsId
        )
        timeline.tsIds
      )
    ]

viewTimespan : Timespan -> Color -> Html Msg
viewTimespan timespan color =
  div
    [ class "tl-timespan"
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

illegalTimespanId : Int -> Html Msg
illegalTimespanId tsId =
  log ("### ERROR: " ++ String.fromInt tsId ++ " is an illegal Timespan ID") text ""

hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"
