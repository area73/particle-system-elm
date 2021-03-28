module RandomExtras exposing (..)

import Random exposing (Seed)

byRange: Seed -> (Float, Float) -> Float
byRange seed (from,to) =
    Random.step (Random.float from to) seed
        |> Tuple.first
