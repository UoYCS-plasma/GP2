module Geometry.Arc exposing (Arc, toSvg, peak)


import Svg
import Svg.Attributes exposing (..)
import Geometry.Vec2 as Vec2 exposing (Vec2)


type alias Arc =
    { start : Vec2
    , end : Vec2
    , major : Float
    , minor : Float
    , sweep : Bool
    }


peak : Arc -> Vec2
peak { start, end, major, minor, sweep } =
    let
        axis =
            Vec2.sub end start

        toCenter =
            sqrt (1 - (Vec2.distance end start)^2/(2*major)^2)

        scale =
            if sweep then
                1 + toCenter

            else
                1 - toCenter
    in
    Vec2.perpendicular axis
        |> Vec2.normalize
        |> Vec2.mul (scale * minor)
        |> Vec2.add (Vec2.div 2 axis)
        |> Vec2.add start


toSvg : Arc -> List (Svg.Attribute msg)
toSvg { start, end, major, minor, sweep } =
    [ d ("M "
        ++ String.fromFloat start.x
        ++ " "
        ++ String.fromFloat start.y
        ++ " A "
        ++ String.fromFloat major
        ++ " "
        ++ String.fromFloat minor
        ++ " "
        ++ (String.fromFloat <| Vec2.angle start end)
        ++ " "
        ++ (if sweep then "1" else "0")
        ++ " 1 "
        ++ String.fromFloat end.x
        ++ " "
        ++ String.fromFloat end.y)
    ] 
