module Model exposing (..)

import Browser.Dom as Dom
import Json.Decode as D
import Json.Decode.Pipeline exposing (required, hardcoded)
import Json.Encode as E
import Dict exposing (Dict)



type alias Model =
  { title : String
  , timelines : Timelines
  , timespans : Timespans
  , selection : SelectionState  -- transient
  , editState : EditState       -- transient
  , dragState : DragState       -- transient
  , settings : Settings
  , nextId : Id
  -- Zooming
  , zoom : Int
  -- Settings dialog
  , isSettingsOpen : Bool       -- transient
  , settingsBuffer :            -- transient
    { beginYear : String
    , endYear : String
    }
  }


type alias Timelines = Dict Id Timeline


type alias Timeline =
  { id : Id
  , title : String
  , color : Hue
  , tsIds : List Id
  }


type alias Timespans = Dict Id Timespan


type alias Timespan =
  { id : Id
  , title : String
  , begin : Int
  , end : Int
  , beginMargin : Int
  , endMargin : Int
  }


type alias TitleDict a = Dict Id { a | title : String }


type DragState
  = NoDrag
  | Engaged Point Class Id
  | DragTimespan Id TimespanMode Point -- timespan id, mode, last point
  | DrawRect Id Point Size -- timeline id, start point, width/height


type TimespanMode
  = MoveTimespan
  | MoveBegin
  | MoveEnd
  | MoveBeginMargin
  | MoveEndMargin


type SelectionState
  = TimelineSelection Id
  | TimespanSelection Id
  | NoSelection


type EditState
  = TimelineEdit Id
  | TimespanEdit Id
  | TitleEdit
  | NoEdit


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


type alias Settings =
  { beginYear : Int
  , endYear : Int
  }


type Field
  = BeginYear
  | EndYear


type ZoomOp
  = In
  | Out


type alias ZoomLevel =
  { pixelPerYear : Int
  , yearStep : Int
  }


type Msg
  = AddTimeline
  | AddTimespan Id Point Size (Result Dom.Error Dom.Element) -- 1st param is timeline id
  | Select SelectionState
  | EditStart EditState
  | Edit String
  | EditEnd
  | MouseDown -- mouse down somewhere
  | MouseDownItem Point Class Id -- mouse down on an item where a drag can be engaged
  | MouseMove Point
  | MouseUp
  | Delete
  -- Zooming
  | Zoom ZoomOp
  -- Settings dialog
  | OpenSettings
  | EditSetting Field String
  | CloseSettings
  --
  | NoOp


defaultModel : Model
defaultModel =
  Model
    "Title"
    ( Dict.fromList
      [ (1, Timeline 1 "A Timeline" 120 []) ] -- green
    )
    ( Dict.fromList
      []
    )
    NoSelection
    NoEdit
    NoDrag
    (Settings 1960 2040)
    2 -- nextId
    3 -- zoom
    False
    { beginYear = "1960"
    , endYear = "2040"
    }



-- ENCODE/DECODE MODEL <-> JS VALUE (for storage)


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
          [ ("id",          E.int    timespan.id)
          , ("title",       E.string timespan.title)
          , ("begin",       E.int    timespan.begin)
          , ("end",         E.int    timespan.end)
          , ("beginMargin", E.int    timespan.beginMargin)
          , ("endMargin",   E.int    timespan.endMargin)
          ]
        )
        model.timespans
      )
    , ("settings", E.object
        [ ("beginYear", E.int model.settings.beginYear)
        , ("endYear",   E.int model.settings.endYear)
        ]
      )
    , ("nextId", E.int model.nextId)
    , ("zoom", E.int model.zoom)
    ]


decoder : D.Decoder Model
decoder =
  D.succeed Model
    |> required "title" D.string
    |> required "timelines"
        (D.dict
          (D.map4 Timeline
            (D.field "id" D.int)
            (D.field "title" D.string)
            (D.field "color" D.int)
            (D.field "tsIds" (D.list D.int))
          ) |> D.andThen strToIntDictDecoder
        )
    |> required "timespans"
        (D.dict
          (D.map6 Timespan
            (D.field "id" D.int)
            (D.field "title" D.string)
            (D.field "begin" D.int)
            (D.field "end" D.int)
            (D.field "beginMargin" D.int)
            (D.field "endMargin" D.int)
          ) |> D.andThen strToIntDictDecoder
        )
    |> hardcoded NoSelection
    |> hardcoded NoEdit
    |> hardcoded NoDrag
    |> required "settings"
        (D.map2 Settings
          (D.field "beginYear" D.int)
          (D.field "endYear" D.int)
        )
    |> required "nextId" D.int
    |> required "zoom" D.int
    |> hardcoded False
    |> hardcoded
        { beginYear = ""
        , endYear = ""
        }
    |> D.andThen initBufferDecoder


strToIntDictDecoder : Dict String v -> D.Decoder (Dict Id v)
strToIntDictDecoder strDict =
  case strToIntDict strDict of
    Just dict -> D.succeed dict
    Nothing -> D.fail "Transformation Dict String -> Int failed"


strToIntDict : Dict String v -> Maybe (Dict Id v)
strToIntDict strDict =
  strDict |> Dict.foldl
    (\k v b ->
      case b of
        Just b_ ->
          case String.toInt k of
            Just i -> Just (Dict.insert i v b_)
            Nothing -> Nothing
        Nothing -> Nothing
    )
    (Just Dict.empty)


initBufferDecoder : Model -> D.Decoder Model
initBufferDecoder model =
  let
    s = model.settings
  in
  D.succeed
    { model | settingsBuffer =
      { beginYear = String.fromInt s.beginYear
      , endYear = String.fromInt s.endYear
      }
    }
