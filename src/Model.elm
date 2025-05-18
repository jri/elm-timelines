module Model exposing (..)

import Browser.Dom as Dom
import Json.Decode as D
import Json.Encode as E
import Dict exposing (Dict)



type alias Model =
  { title : String
  , timelines : Timelines
  , timespans : Timespans
  , dragState : DragState -- transient
  , selection : Target -- transient
  , editState : Target -- transient
  , settings : Settings
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
  = NoDrag
  | Engaged Point Class Id
  | DragTimespan Id TimespanMode Point -- timespan id, mode, last point
  | DrawRect Id Point Size -- timeline id, start point, width/height


type TimespanMode
  = MoveTimespan
  | MoveBegin
  | MoveEnd


type Target
  = TimelineTarget Id
  | TimespanTarget Id
  | NoTarget


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


type Msg
  = AddTimeline
  | AddTimespan Id Point Size (Result Dom.Error Dom.Element) -- 1st param is timeline id
  | Select Target
  | EditStart Target
  | Edit String
  | EditEnd
  | MouseDown -- mouse down somewhere
  | MouseDownItem Point Class Id -- mouse down on an item where a drag can be initiated
  | MouseMove Point
  | MouseUp
  | Delete
  | NoOp


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
    NoDrag
    NoTarget
    NoTarget
    { beginYear = 1985
    , endYear = 2025
    }
    100



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
          [ ("id",    E.int    timespan.id)
          , ("title", E.string timespan.title)
          , ("begin", E.int    timespan.begin)
          , ("end",   E.int    timespan.end)
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
    ]


decoder : D.Decoder Model
decoder = D.map8 Model
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
  (D.succeed NoDrag)
  (D.succeed NoTarget)
  (D.succeed NoTarget)
  (D.field "settings"
    (D.map2 Settings
      (D.field "beginYear" D.int)
      (D.field "endYear"   D.int)
    )
  )
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
