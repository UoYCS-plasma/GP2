module Graphics.Colour exposing (Colour, brighten, toCss)


type alias Colour =
    { r : Int
    , g : Int
    , b : Int
    , a : Float
    }


brighten : Int -> Colour -> Colour
brighten amount colour =
    { colour
        | r = colour.r + amount
        , g = colour.g + amount
        , b = colour.b + amount
    }


toCss : Colour -> String
toCss colour =
    "rgba("
        ++ String.fromInt colour.r
        ++ ","
        ++ String.fromInt colour.g
        ++ ","
        ++ String.fromInt colour.b
        ++ ","
        ++ String.fromFloat colour.a
        ++ ")"
