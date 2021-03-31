module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, Shape, circle, rect, shapes, toHtml)
import Canvas.Settings exposing (fill)
import Color exposing (Color, fromRgba)
import Dict
import Flip exposing (flip)
import Html exposing (Html, div, h1, text)
import Http exposing (Error(..), Expect, expectStringResponse)
import List exposing (concat, foldl, foldr, head, length, range, reverse)
import Loop
import PSUtils exposing (randomColor)
import Random exposing (Seed, independentSeed, initialSeed)
import Random.Extra as Random
import Random.Float exposing (anyFloat)
import RandomExtras

import TypeDefinitions exposing (Rgba)
import Emitter exposing (Emitter)
import Field exposing (Field)
import Particle exposing (Particle)
import Vector exposing (Vector, add, disturbanceAccelerationFactor)
import Typeclasses.Classes.Semigroup exposing (..)


-- STRUCTS


type alias Config =
    { bgColor : Rgba }


-- MODEL


type alias Flags =
    { emitters : List Emitter
    , fields : List Field
    , particles : List Particle
    }


type Model
    = Failure Error
    | Loading String
    | Success
        { frameRate : Float
        , count : Int
        , data : Flags
        }


type Msg
    = Frame Float



-- INIT

product: Float -> Float -> Float
product a b =
    a * b

randomRotationAngle : Seed -> Float -> Float
randomRotationAngle seed angle =
     RandomExtras.byRange seed (-1, 1)
        |> product angle

velocityFromEmitter: Seed ->  Emitter -> Vector
velocityFromEmitter seed {spread , velocity} =
    randomRotationAngle seed spread
    |> degrees
    |> flip Vector.rotate velocity

-- TODO : refactor


tupleSeedEmitter : Seed -> Emitter -> ( Seed, List Particle ) -> ( Seed, List Particle )
tupleSeedEmitter seed emitter tuple =
    ( Random.initialSeed (length (Tuple.second tuple)), createParticleFromEmitter seed emitter :: Tuple.second tuple)

groupParticlesFromEmitter : Seed -> Emitter -> List Particle
groupParticlesFromEmitter seed emitter =
    let
        addNum: Int -> Int -> Int
        addNum a b = a + b
    in
    List.range 1 emitter.density
    |>List.indexedMap (\v -> addNum (RandomExtras.byRangeInt seed (1, 10000)))
    |>Debug.log "NUMS"
    |>List.map Random.initialSeed

    |>List.map (flip createParticleFromEmitter emitter)


createParticleFromEmitter : Seed -> Emitter -> Particle
createParticleFromEmitter seed emitter =
    { position = emitter.position
    , velocity = velocityFromEmitter seed emitter
    , acceleration = { x = 0, y = 0}
    , color = randomColor seed
    , size = 1
    }


init : Flags -> ( Model, Cmd Msg )
init data =
    ( Success { frameRate = 0, count = 1, data = data }, Cmd.none )

updateAcceleration: List Field -> Particle -> Particle
updateAcceleration fields particle =
    let
        attractionForce: Field -> Vector
        attractionForce field =
            let
               difference = Vector.substract field.position particle.position
               disturbance =  disturbanceAccelerationFactor field particle
            in
            Vector.scalar difference disturbance

        acceleration =
            List.map attractionForce fields
                |> List.foldl Vector.add {x = 0, y = 0}
                |> Vector.opposite

    in
       {particle | acceleration = acceleration }

-- UPDATE

subscriptions _ =
    onAnimationFrameDelta Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
{-    let
        frameRate =
            case msg of
                Frame v ->
                    v
    in
    -}
    case model of
        Failure err ->
            ( model, Cmd.none )

        Loading str ->
            ( model, Cmd.none )

        Success {frameRate , count, data} ->
            let
                {emitters, particles, fields} = data
                seed0 =
                    Random.initialSeed count

                particlesGroup : Emitter -> List Particle
                particlesGroup emitter =
                    groupParticlesFromEmitter seed0 emitter


                addParticlesFromEmitters : List Emitter -> List Particle
                addParticlesFromEmitters emittersList =
                    List.concatMap particlesGroup emittersList

                updatePosition: Particle -> Vector
                updatePosition particle =
                    Vector.add particle.position particle.velocity
                        |> Vector.add particle.acceleration

                moveParticle : Particle -> Particle
                moveParticle particle =
                    { particle | position = updatePosition particle }

                -- NEW DATA BEEN ADDED TO DATA FIELD
                newParticles : List Particle
                newParticles =
                    addParticlesFromEmitters emitters
                        ++ particles
                        -- we are limiting the num fo particles  later we should remove the ones that goes beyond boundaries
                        |> limitParticles 5000
                        -- Apply disturbance to particles and update position
                        |> List.map ((updateAcceleration fields) >> moveParticle)

                newData =
                    { particles = newParticles
                    , emitters = emitters
                    , fields = fields
                    }
            in
            ( Success { frameRate = frameRate, count = count + 1, data = newData }, Cmd.none )


--VIEW


width =
    1200


height =
    1200


view : Model -> Document Msg
view model =
    { title = "Particle System"
    , body =
        [ toHtml ( width, height )
            []
            (shapes [ fill Color.black ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]
               :: modelToShape model
            )
        , div [] [ text "this is it" ]
        ]
    }

limitParticles: Int -> List Particle -> List Particle
limitParticles num list =
    List.take num list

modelToShape : Model -> List Renderable
modelToShape model =
    case model of
        Failure e ->
            [ shapes [] [] ]

        Loading s ->
            [ shapes [] [] ]

        Success m ->
            fieldShapes m.data.fields ++ particleShapes m.data.particles


convertToCircleField : Field -> Shape
convertToCircleField field =
    circle ( field.position.x, field.position.y ) (field.size / 2)

convertToCircleParticle : Particle -> Shape
convertToCircleParticle particle =
    circle ( particle.position.x, particle.position.y ) (particle.size / 2)



defaultColor : Rgba
defaultColor =
    { red = 0, green = 1, blue = 0, alpha = 1 }


convertFieldToShapes : Field -> Renderable
convertFieldToShapes field =
    shapes
        [ fill
            (Color.fromRgba
                (field.color
                    |> head
                    |> Maybe.withDefault defaultColor
                )
            )
        ]
        [ convertToCircleField field ]


convertParticleToShapes : Particle -> Renderable
convertParticleToShapes particle =
    shapes
        [ fill (Color.fromRgba particle.color)]
        [ convertToCircleParticle particle ]


fieldShapes : List Field -> List Renderable
fieldShapes jsonFields =
    List.map convertFieldToShapes jsonFields


particleShapes : List Particle -> List Renderable
particleShapes jsonParticles =
    List.map convertParticleToShapes jsonParticles


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
