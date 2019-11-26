module Geometry.LineSegment exposing (LineSegment, length, toSvg)

import Geometry.Vec2 as Vec2 exposing (Vec2)
import Svg
import Svg.Attributes exposing (..)

type alias LineSegment =
    { start : Vec2
    , end : Vec2
    }


length : LineSegment -> Float
length { start, end } =
    Vec2.distance start end


toSvg : LineSegment -> List (Svg.Attribute msg)
toSvg { start, end } =
    [ x1 (String.fromFloat start.x)
    , y1 (String.fromFloat start.y)
    , x2 (String.fromFloat end.x)
    , y2 (String.fromFloat end.y)
    ]
