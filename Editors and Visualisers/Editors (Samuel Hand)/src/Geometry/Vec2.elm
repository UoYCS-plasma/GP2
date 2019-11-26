module Geometry.Vec2 exposing (Vec2, magnitude, add, sub, mul, div, normalize, angle, distance, direction, perpendicular)


type alias Vec2 =
    { x : Float, y : Float }


-- Vector magnitude
magnitude : Vec2 -> Float
magnitude v =
    sqrt <| v.x^2 + v.y^2


-- Vector addition
add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
    { v1 | x = v1.x + v2.x, y = v1.y + v2.y }


-- Vector subtraction
sub : Vec2 -> Vec2 -> Vec2
sub v1 v2 =
    { v1 | x = v1.x - v2.x, y = v1.y - v2.y }


-- Scalar multiplication
mul : Float -> Vec2 -> Vec2
mul s v =
    { v | x = s * v.x, y = s * v.y }


-- Scalar division
div : Float -> Vec2 -> Vec2
div s v =
    mul (1 / s) v


-- Find the unit vector with the same direction
normalize : Vec2 -> Vec2
normalize v =
    if magnitude v == 0 then
        { x = 0, y = 0 }
    
    else
        div (magnitude v) v 


perpendicular : Vec2 -> Vec2
perpendicular { x, y } =
    { x = y, y = -x }


-- Find the angle between two vectors
angle : Vec2 -> Vec2 -> Float
angle v1 v2 =
    (180 / pi) * (atan <| (v2.y - v1.y) / (v2.x - v1.x))


-- Find the distance between two position vectors
distance : Vec2 -> Vec2 -> Float
distance v1 v2 =
    sub v2 v1 |> magnitude


-- Find the normalized direction vector between two position vectors
direction : Vec2 -> Vec2 -> Vec2
direction v1 v2 =
    sub v2 v1 |> normalize
