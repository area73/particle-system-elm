module Vector exposing (Vector, scalar, add, rotate, distance, opposite)

import Tuple exposing (first, second)
type alias Vector =
    { x : Float
    , y : Float
    }


add : Vector -> Vector -> Vector
add a b =
   {x = a.x + b.x, y = a.y + b.y}

multiply :  Vector -> Vector -> Vector
multiply a b =
          {x = a.x * b.x, y = a.y * b.y}

scalar :  Vector -> Float -> Vector
scalar a k =
          {x = a.x * k, y = a.y * k}

fromTuple: (Float, Float ) -> Vector
fromTuple (a,b) =
    {x = a, y = b}

rotate : Float -> Vector -> Vector
rotate ang {x,y} =
    toPolar ( x, y )
        |> Tuple.mapSecond ((+) ang)
        |> fromPolar
        |> fromTuple

opposite: Vector -> Vector
opposite {x,y} =
    {x = x * -1 , y = y * -1}

distance: Vector -> Float
distance {x,y} = sqrt (x^2 + y^2)
