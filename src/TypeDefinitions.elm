module TypeDefinitions exposing (Emitter, Field, Particle, Rgba, rgba)

import Color exposing (Color, fromRgba)
import Json.Decode exposing (Decoder, field, float, map4)
import Vector exposing (Vector)






{--
type alias Vector =
    { v0 : Point
    , v1 : Point
    }
--}


rgba : Decoder Rgba
rgba =
    map4 Rgba
        (field "red" float)
        (field "green" float)
        (field "blue" float)
        (field "alpha" float)


type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , color : Rgba
    , size : Int
    , gravity : Float
    }


type alias Emitter =
    { id : String
    , position : Vector
    , spread : Float
    , velocity : Vector
    , color : List Rgba
    , size : Int
    , density : Int
    }


type alias Rgba =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias Field =
    { id : String
    , position : Vector
    , color : List Rgba
    , size : Int
    , density : Float
    }
