module Main exposing (..)

import Browser
import Html exposing (Html, h1, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = TimeBoard

type alias TimeBoard =
  { title : String
  , timelines : List Timeline
  }

type alias Timeline = 
  { title : String
  , color : Color
  , entries : List Timespan
  }

type alias Timespan =
  { title : String
  , begin : Int
  , end : Int
  }

type alias Color = Int

init : Model
init =
  TimeBoard "Terry's life"
    [ Timeline "Living places" 120 -- green
      [ Timespan "Park Avenue" 200 350
      , Timespan "Lake Street" 400 700
      ]
    , Timeline "Girlfriends" 0 -- red
      [ Timespan "Barbara" 150 315
      , Timespan "Caroline" 320 350
      , Timespan "Marina" 500 600
      ]
    ]



-- UPDATE


type Msg
  = None

update : Msg -> Model -> Model
update msg model = model



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "font-family" "sans-serif"
    , style "margin" "20px"
    ]
    [ h1 [] [ text model.title ]
    , div [] ( List.map viewTimeline model.timelines )
    ]

viewTimeline : Timeline -> Html Msg
viewTimeline timeline =
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
      ( List.map ( \t -> viewTimespan t timeline.color ) timeline.entries )
    ]

viewTimespan : Timespan -> Color -> Html Msg
viewTimespan timespan color =
  div
    [ style "position" "absolute"
    , style "top" "0"
    , style "left" <| String.fromInt timespan.begin ++ "px"
    , style "width" <| String.fromInt ( timespan.end - timespan.begin ) ++ "px"
    , style "height" "100%"
    , style "padding" "5px"
    , style "box-sizing" "border-box"
    , style "background-color" (hsl color "90%")
    ]
    [ span [] [ text timespan.title ]]

hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"
