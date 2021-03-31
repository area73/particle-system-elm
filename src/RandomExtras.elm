module RandomExtras exposing (byRange,byRangeInt)

import Random exposing (Seed)

byRange: Seed -> (Float, Float) -> Float
byRange seed (from,to) =
    Random.step (Random.float from to) seed
        |> Tuple.first

byRangeInt seed (from,to) =
    Random.step (Random.int from to) seed
        |> Tuple.first
