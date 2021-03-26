module TypeDefinitions exposing (Rgba)

import Color exposing (Color, fromRgba)
import Json.Decode exposing (Decoder, field, float, map4)
import Vector exposing (Vector)






{--
type alias Vector =
    { v0 : Point
    , v1 : Point
    }
--}


{-
rgba : Decoder Rgba
rgba =
    map4 Rgba
        (field "red" float)
        (field "green" float)
        (field "blue" float)
        (field "alpha" float)
-}


type alias Rgba =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
