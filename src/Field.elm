module Field exposing (Field)

import TypeDefinitions exposing (Rgba)
import Vector exposing (Vector)


type alias Field =
    { id : String
    , position : Vector
    , color : List Rgba
    , size : Float
    , density : Float
    }
