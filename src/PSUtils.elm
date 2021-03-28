module PSUtils exposing (randomColor)

import Field exposing (Field)
import Particle exposing (Particle)
import Random exposing (Seed)
import TypeDefinitions exposing (Rgba)
import Vector exposing (Vector, distance, substract)

randomColor: Seed -> Rgba
randomColor seed0 =
    let
        (red, seed1) = Random.step (Random.float 0 1) seed0
        (green, seed2) = Random.step (Random.float 0 1) seed1
        (blue, seed3) = Random.step (Random.float 0 1) seed2
        (alpha, seed4) = Random.step (Random.float 0 1) seed3
    in
    { red = red, green = green, blue=blue, alpha=alpha}

