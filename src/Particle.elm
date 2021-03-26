module Particle exposing (Particle)
import TypeDefinitions exposing (Rgba)
import Vector exposing (Vector)

type alias Particle =
    { position : Vector
    , velocity : Vector
    , acceleration : Vector
    , color : Rgba
    , size : Int
    }
