module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Array



-- CONFIG


conf =
  { primaryFontSize = "16px"
  , secondaryFontSize = "14px"
  , pixelPerYear = 64
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
  , style "font-size" conf.primaryFontSize
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
  , style "font-size" conf.secondaryFontSize
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
  , style "font-size" conf.secondaryFontSize
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
      if isSelected model id then
        conf.selectionColor
      else
        "transparent"      
  in
  [ style "border" color ]


toolbarStyle : List (Attribute Msg)
toolbarStyle =
  [ style "margin-top" "26px" ]


toolbarButtonStyle : List (Attribute Msg)
toolbarButtonStyle =
  [ style "font-size" conf.secondaryFontSize ]


nextToolbarButtonStyle : List (Attribute Msg)
nextToolbarButtonStyle =
  [ style "margin-left" "26px" ]


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


inlineEditStyle : EditTarget -> List (Attribute Msg)
inlineEditStyle target =
  let
    (fs, fw) = case target of
      TimelineEdit _ -> ( conf.secondaryFontSize, "bold" )
      TimespanEdit _ -> ( conf.primaryFontSize, "normal" )
      TitleEdit -> ( "32px", "bold" ) -- TODO
      NoEdit -> ( "", "" ) -- error logged already by caller
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
    x = toRenderValue timespan.begin - conf.pixelPerYear * (beginYear - 1970)
    width = toRenderValue (timespan.end - timespan.begin)
  in
    (x, width)


toRenderValue : Int -> Int
toRenderValue width =
  conf.pixelPerYear * width // 365


toModelSpace : Model -> Int -> Int -> (Int, Int)
toModelSpace model x width =
  let
    beginYear = model.settings.beginYear
    begin = toModelValue x + 365 * (beginYear - 1970)
    end = begin + toModelValue width
  in
    (begin, end)


toModelValue : Int -> Int
toModelValue width =
  365 * width // conf.pixelPerYear


isSelected : Model -> Id -> Bool
isSelected model id =
  case model.selection of
    TimelineSelection id_ -> id_ == id
    TimespanSelection id_ -> id_ == id
    NoSelection -> False


hsl : Int -> String -> String
hsl hue lightness =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ")"
