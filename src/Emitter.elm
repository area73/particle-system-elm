module Emitter exposing (Emitter)

import TypeDefinitions exposing (Rgba)
import Vector exposing (Vector)

type alias Emitter =
    { id : String
    , position : Vector
    , spread : Float
    , velocity : Vector
    , color : List Rgba
    , size : Int
    , density : Int
    }
