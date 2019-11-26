module Geometry.Ellipse exposing (Ellipse, contains, fromCircle, intersects, lineBetween, project, setCenter, setMajor, setMinor, toSvg, upperArc)

import Geometry.LineSegment exposing (LineSegment)
import Geometry.Vec2 as Vec2 exposing (Vec2)
import Svg
import Svg.Attributes exposing (..)


type alias Ellipse =
    { center : Vec2, major : Float, minor : Float }


fromCircle : Float -> Vec2 -> Ellipse
fromCircle radius center =
    { center = center, major = radius, minor = radius }


setCenter : Vec2 -> Ellipse -> Ellipse
setCenter new ellipse =
    { ellipse | center = new }


setMajor : Float -> Ellipse -> Ellipse
setMajor new ellipse =
    { ellipse | major = new }


setMinor : Float -> Ellipse -> Ellipse
setMinor new ellipse =
    { ellipse | minor = new }


contains : Vec2 -> Ellipse -> Bool
contains v { center, major, minor } =
    let
        translated =
            Vec2.sub v center
    in
    (translated.x / major) ^ 2 + (translated.y / minor) ^ 2 <= 1


lineBetween : Ellipse -> Ellipse -> LineSegment
lineBetween e1 e2 =
    { start = project e2.center e1, end = project e1.center e2 }


project : Vec2 -> Ellipse -> Vec2
project v { center, major, minor } =
    let
        translated =
            Vec2.sub v center

        scale =
            if not (translated.x == 0 && translated.y == 0) then
                (major * minor)
                    / (sqrt <| (major * translated.y) ^ 2 + (minor * translated.x) ^ 2)

            else
                0
    in
    Vec2.mul scale translated |> Vec2.add center


upperArc : Ellipse -> ( Vec2, Vec2 )
upperArc { center, major, minor } =
    let
        x1 =
            -(major * minor) / (sqrt <| major ^ 2 + minor ^ 2)
    in
    ( Vec2.add center { x = x1, y = x1 }, Vec2.add center { x = -x1, y = x1 } )


intersects : LineSegment -> Ellipse -> Bool
intersects { start, end } { center, major, minor } =
    let
        start_ =
            Vec2.sub start center

        end_ =
            Vec2.sub end center

        a =
            (end_.x - start_.x) ^ 2 / major ^ 2 + (end_.y - start_.y) ^ 2 / minor ^ 2

        b =
            2 * (start_.x * (end_.x - start_.x) / major ^ 2 + start_.y * (end_.y - start_.y) / minor ^ 2)

        c =
            start_.x ^ 2 / major ^ 2 + start_.y ^ 2 / minor ^ 2 - 1

        discriminant =
            b ^ 2 - 4 * a * c

        sol1 =
            (-b + sqrt discriminant) / (2 * a)

        sol2 =
            (-b - sqrt discriminant) / (2 * a)
    in
    discriminant > 0 && ((0 <= sol1 && sol1 <= 1) || (0 <= sol2 && sol2 <= 1))


toSvg : Ellipse -> List (Svg.Attribute msg)
toSvg ellipse =
    [ cx <| String.fromFloat ellipse.center.x
    , cy <| String.fromFloat ellipse.center.y
    , rx <| String.fromFloat ellipse.major
    , ry <| String.fromFloat ellipse.minor
    ]
