module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Array



-- CONFIG


conf =
  { pixelPerYear = 64
  , timelineColors = Array.fromList [120, 0, 210, 36, 270, 58]
                               -- green, red, blue, orange, purple, yellow
  , selectionColor = "#007AFF" -- Firefox focus color
  }



-- STYLE


appStyle : Model -> List (Attribute Msg)
appStyle model =
  let
    userSelect =
      case model.dragState of
        NoDrag -> "auto"
        _ -> "none"
  in
  [ style "font-family" "sans-serif"
  , style "font-size" "16px"
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "padding" "20px"
  , style "box-sizing" "border-box"
  , style "user-select" userSelect
  ]


contentStyle : List (Attribute Msg)
contentStyle =
  [ style "display" "flex"
  , style "overflow" "auto"
  ]


blinderStyle : List (Attribute Msg)
blinderStyle =
  [ style "position" "absolute"
  , style "width" "150px" -- corresponds to timeline header width
  , style "height" "30px" -- corresponds to timescale height
  , style "z-index" "2" -- place on top of horizontal/vertical sticky areas
  , style "background-color" "white"
  --, style "background" "repeating-linear-gradient(-45deg, cyan, white 20px)"
  ]


timelineHeadersStyle : List (Attribute Msg)
timelineHeadersStyle =
  [ style "position" "sticky"
  , style "left" "0"
  , style "z-index" "1"
  , style "margin-top" "30px" -- corresponds to timescale height
  ]


timelineHeaderStyle : Timeline -> List (Attribute Msg)
timelineHeaderStyle timeline =
  [ style "background-color" (hsl timeline.color "95%")
  , style "font-size" "14px"
  , style "font-weight" "bold"
  , style "width" "150px"
  , style "height" "60px"
  , style "margin-top" "5px"
  , style "padding" "5px 6px"
  , style "box-sizing" "border-box"
  ]


timeScaleStyle : List (Attribute Msg)
timeScaleStyle =
  [ style "position" "sticky"
  , style "top" "0"
  , style "z-index" "1"
  , style "font-size" "14px"
  , style "margin-bottom" "1px"
  , style "background-color" "white"
  ]


timelineStyle : Timeline -> List (Attribute Msg)
timelineStyle timeline =
  [ style "position" "relative"
  , style "height" "60px"
  , style "margin-bottom" "5px"
  , style "background-color" (hsl timeline.color "95%")
  , style "cursor" "crosshair"
  ]


timespanBorderStyle : Model -> Timespan -> List (Attribute Msg)
timespanBorderStyle model timespan =
  let
    (left, width) = toRenderSpace model timespan
  in
  [ style "position" "absolute"
  , style "top" "0"
  , style "left" (String.fromInt left ++ "px")
  , style "width" (String.fromInt width ++ "px")
  , style "height" "100%"
  , style "box-sizing" "border-box"
  ]
  ++ selectionBorderStyle model timespan.id


timespanStyle : Model -> Hue -> List (Attribute Msg)
timespanStyle model hue =
  let
    cursor =
      case model.dragState of
        DragTimespan _ _ _ -> "grabbing"
        _ -> "grab"
  in
  [ style "height" "100%"
  , style "padding" "5px 6px"
  , style "box-sizing" "border-box"
  , style "background-color" (hsl hue "60%")
  , style "opacity" "0.5"
  , style "cursor" cursor
  ]


resizerStyle : String -> List (Attribute Msg)
resizerStyle pos =
  [ style "position" "absolute"
  , style "top" "0"
  , style pos "-5px"
  , style "width" "10px"
  , style "height" "100%"
  , style "z-index" "1" -- keeps cursor when hovering other timespan
  , style "cursor" "col-resize"
  -- , style "background-color" "hsl(0, 0%, 50%)" -- for debugging
  -- , style "opacity" "0.2"
  ]


selectionBorderStyle : Model -> Id -> List (Attribute Msg)
selectionBorderStyle model id =
  let
    color = "2px solid " ++
      if isActive model .selection id then
        conf.selectionColor
      else
        "transparent"      
  in
  [ style "border" color ]


rectangleStyle : Point -> Size -> List (Attribute Msg)
rectangleStyle p size =
  [ style "position" "absolute"
  , style "top" (String.fromInt p.y ++ "px")
  , style "left" (String.fromInt p.x ++ "px")
  , style "width" (String.fromInt size.width ++ "px")
  , style "height" (String.fromInt size.height ++ "px")
  , style "border" "1px dashed gray"
  , style "cursor" "crosshair"
  ]


inlineEditStyle : Target -> List (Attribute Msg)
inlineEditStyle target =
  let
    (fs, fw) = case target of
      TimelineTarget _ -> ( "14px", "bold" )
      TimespanTarget _ -> ( "16px", "normal" )
      NoTarget -> ( "", "" ) -- error logged already by caller
  in
  [ style "position" "relative"
  , style "top" "-3px"
  , style "left" "-4px"
  , style "width" "130px"
  , style "font-family" "sans-serif" -- Default (on Mac) is "-apple-system"
  , style "font-size" fs
  , style "font-weight" fw
  ]



-- HELPER


toRenderSpace : Model -> Timespan -> (Int, Int)
toRenderSpace model timespan =
  let
    beginYear = model.settings.beginYear
    ppy = conf.pixelPerYear
    x = ppy * timespan.begin // 365 - ppy * (beginYear - 1970)
    width = ppy * (timespan.end - timespan.begin) // 365
  in
    (x, width)


toModelSpace : Model -> Int -> Int -> (Int, Int)
toModelSpace model x width =
  let
    beginYear = model.settings.beginYear
    ppy = conf.pixelPerYear
    begin = 365 * x // ppy + 365 * (beginYear - 1970)
    end = begin + toModelWidth width
  in
    (begin, end)


toModelWidth : Int -> Int
toModelWidth width =
  365 * width // conf.pixelPerYear


isActive : Model -> (Model -> Target) -> Id -> Bool
isActive model targetFunc id =
  case targetFunc model of
    TimelineTarget id_ -> id_ == id
    TimespanTarget id_ -> id_ == id
    NoTarget -> False


hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"
