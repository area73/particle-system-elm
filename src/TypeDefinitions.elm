module TypeDefinitions exposing (Emitter, Field, Particle, Point, Rgba, rgba)

import Color exposing (Color, fromRgba)
import Json.Decode exposing (Decoder, field, float, map4)


type alias Point =
    { x : Float
    , y : Float
    , z : Float
    }



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
    { position : Point
    , velocity : Point
    , acceleration : Point
    , color : Rgba
    , size : Int
    , gravity : Float
    }


type alias Emitter =
    { id : String
    , position : Point
    , spread : Float
    , velocity : Point
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
    , position : Point
    , color : List Rgba
    , size : Int
    , density : Float
    }
