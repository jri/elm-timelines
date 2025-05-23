module Style exposing (..)

import Model exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Array
import Debug exposing (log)



-- CONFIG


conf =
  { primaryFontSize = "16px"
  , secondaryFontSize = "14px"
  , timelineColors = Array.fromList [120, 0, 210, 36, 270, 58]
                               -- green, red, blue, orange, purple, yellow
  , selectionColor = "#007AFF" -- Firefox focus color
  , zoomLevels = Array.fromList
      [ ZoomLevel 512  1    -- 0 max in
      , ZoomLevel 256  1    -- 1
      , ZoomLevel 128  1    -- 2
      , ZoomLevel  64  1    -- 3 default
      , ZoomLevel  32  2    -- 4
      , ZoomLevel  16  5    -- 5
      , ZoomLevel   8 10    -- 6 max out
      ]
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
  , style "display" "flex"
  , style "flex-direction" "column"
  , style "height" "100%"
  , style "padding" "20px"
  , style "box-sizing" "border-box"
  , style "user-select" userSelect
  ]
  ++ defaultTextStyle


defaultTextStyle : List (Attribute Msg)
defaultTextStyle =
  [ style "font-size" conf.primaryFontSize
  , style "font-weight" "normal"
  ]


titleTextStyle : List (Attribute Msg)
titleTextStyle =
  [ style "font-size" "24px"
  , style "font-weight" "bold"
  ]


timelineHeaderTextStyle : List (Attribute Msg)
timelineHeaderTextStyle =
  [ style "font-size" conf.secondaryFontSize
  , style "font-weight" "bold"
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


timespanStyle : Model -> Timespan -> Hue -> List (Attribute Msg)
timespanStyle model timespan hue =
  let
    (x, width) = toRenderSpace model timespan.begin timespan.end
    beginWidth = toRenderValue model timespan.beginMargin
    endWidth = toRenderValue model timespan.endMargin
    color = hsla hue "60%" 0.5
    cursor =
      case model.dragState of
        DragTimespan _ _ _ -> "grabbing"
        _ -> "grab"
  in
  [ style "position" "absolute"
  , style "top" "0"
  , style "left" (String.fromInt (x - beginWidth) ++ "px")
  , style "width" (String.fromInt (width + beginWidth + endWidth) ++ "px")
  , style "height" "100%"
  , style "padding" "5px 6px"
  , style "box-sizing" "border-box"
  , style "background" ("linear-gradient(to right, transparent, " ++ color ++ " " ++
      String.fromInt beginWidth ++ "px, " ++ color ++ " " ++
      String.fromInt (beginWidth + width) ++ "px, transparent 100%)")
  , style "cursor" cursor
  ]
  ++ selectionBorderStyle model timespan.id


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
  [ style "display" "flex"
  , style "column-gap" "26px"
  , style "margin-top" "26px"
  ]


toolbarButtonStyle : List (Attribute Msg)
toolbarButtonStyle =
  [ style "font-size" conf.secondaryFontSize ]


spacerStyle : List (Attribute Msg)
spacerStyle =
  [ style "flex" "auto" ]


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


settingsStyle : List (Attribute Msg)
settingsStyle =
  [ style "position" "absolute"
  , style "bottom" "50px"
  , style "right" "20px"
  , style "padding" "15px"
  , style "font-size" conf.secondaryFontSize
  , style "background-color" "white"
  , style "box-shadow" "0 0 4px lightgray"
  ]


settingsContentStyle : List (Attribute Msg)
settingsContentStyle =
  [ style "display" "grid"
  , style "grid-template-columns" "42px 50px"
  , style "gap" "6px"
  , style "align-items" "baseline"
  , style "margin-top" "12px"
  ]


settingsButtonStyle : List (Attribute Msg)
settingsButtonStyle =
  [ style "position" "absolute"
  , style "top" "2px"
  , style "right" "2px"
  ]


editStyle : List (Attribute Msg)
editStyle =
  [ style "position" "relative"
  , style "top" "-3px"
  , style "left" "-4px"
  , style "width" "130px"
  , style "font-family" "sans-serif" -- Default for <input> is "-apple-system" (on Mac)
  ]



-- HELPER


toRenderSpace : Model -> Int -> Int -> (Int, Int)
toRenderSpace model begin end =
  let
    beginYear = model.settings.beginYear
    x = toRenderValue model begin - pixelPerYear model * (beginYear - 1970)
    width = toRenderValue model (end - begin)
  in
    (x, width)


toModelSpace : Model -> Int -> Int -> (Int, Int)
toModelSpace model x width =
  let
    beginYear = model.settings.beginYear
    begin = toModelValue model x + 365 * (beginYear - 1970)
    end = begin + toModelValue model width
  in
    (begin, end)


toRenderValue : Model -> Int -> Int
toRenderValue model x =
  pixelPerYear model * x // 365


toModelValue : Model -> Int -> Int
toModelValue model x =
  365 * x // pixelPerYear model


pixelPerYear : Model -> Int
pixelPerYear model =
  case Array.get model.zoom conf.zoomLevels of
    Just level -> level.pixelPerYear
    Nothing -> logError "pixelPerYear" (String.fromInt model.zoom ++ " is an invalid zoom") 1


yearStep : Model -> Int
yearStep model =
  case Array.get model.zoom conf.zoomLevels of
    Just level -> level.yearStep
    Nothing -> logError "yearStep" (String.fromInt model.zoom ++ " is an invalid zoom") 1


isSelected : Model -> Id -> Bool
isSelected model id =
  case model.selection of
    TimelineSelection id_ -> id_ == id
    TimespanSelection id_ -> id_ == id
    NoSelection -> False


hsl : Int -> String -> String
hsl hue lightness =
  hsla hue lightness 1.0


hsla : Int -> String -> Float -> String
hsla hue lightness alpha =
  "hsl(" ++ String.fromInt hue ++ ", 100%, " ++ lightness ++ ", " ++ String.fromFloat alpha ++ ")"



-- DEBUG


logError : String -> String -> a -> a
logError func text val =
  log ("ERROR in " ++ func ++ ": " ++ text) val
