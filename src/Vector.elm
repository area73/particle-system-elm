module Vector exposing (Vector, add)

type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


add : Vector -> Vector -> Vector
add a b =
   {x = a.x + b.x, y = a.y + b.y, z = a.z + b.z}

multiply :  Vector -> Vector -> Vector
multiply a b =
          {x = a.x * b.x, y = a.y * b.y, z = a.z * b.z}

scalar :  Vector -> Float -> Vector
scalar a k =
          {x = a.x * k, y = a.y * k, z = a.z * k}
