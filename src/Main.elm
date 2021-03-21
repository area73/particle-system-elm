module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, Shape, circle, shapes, toHtml)
import Canvas.Settings exposing (fill)
import Color exposing (Color, fromRgba)
import Html exposing (Html, div, h1, text)
import Http exposing (Error(..), Expect, expectStringResponse)
import Json.Decode as Decode exposing (Decoder, array, decodeString, errorToString, field, float, int, list, map, map3, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List exposing (foldr, head, length)
import Loop
import PseudoRandom exposing (floatSequence)
import Random exposing (Seed, float)
import TypeDefinitions exposing (Emitter, Field, Particle, Point, Rgba, rgba)



-- STRUCTS


type alias Config =
    { bgColor : Rgba }


config =
    { bgColor = { red = 0, green = 0, blue = 0, alpha = 1 } }



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


point : Decoder Point
point =
    map3 Point
        (field "x" Decode.float)
        (field "y" Decode.float)
        (field "z" Decode.float)


emitterDecoder : Decoder Emitter
emitterDecoder =
    Decode.succeed Emitter
        |> required "id" string
        |> required "position" point
        |> required "spread" Decode.float
        |> required "velocity" point
        |> required "color" (list rgba)
        |> required "size" int
        |> required "density" int


lookupData : Flags -> ( Model, Cmd Msg )
lookupData data =
    --procesamos los datos
    -- devolvemos loaded
    let
        _ =
            Debug.log "Parsing data:" data
    in
    ( Success
        { frameRate = 0
        , count = 1
        , data = data
        }
    , Cmd.none
    )


addAngle : Float -> ( Float, Float ) -> ( Float, Float )
addAngle delta ( r, ang ) =
    ( r, ang + delta )


toPoint : ( Float, Float ) -> Point
toPoint ( x, y ) =
    { x = x, y = y, z = toFloat 0 }


rotateVector : Float -> Point -> Point
rotateVector radAng p =
    -- convertimos el punto a polar
    toPolar ( p.x, p.y )
        -- sumamos el ángulo
        |> addAngle radAng
        |> fromPolar
        |> toPoint



-- convertimos el ángulo a radianes
-- sumamos los 2 ángulos
--lo convertimos a cartesiano


generateRandomNum : Seed -> Float -> Float
generateRandomNum seed angle =
    Random.step (Random.float -1 1) seed
        |> Tuple.first
        |> (\v -> v * angle)


tupleSeedEmitter : Seed -> Emitter -> ( Seed, List Particle ) -> ( Seed, List Particle )
tupleSeedEmitter seed emitter tuple =
    ( Random.initialSeed (length (Tuple.second tuple)), createParticleFromEmitter (Random.initialSeed (length (Tuple.second tuple))) emitter :: Tuple.second tuple )


groupParticlesFromEmitter : Seed -> Emitter -> List Particle
groupParticlesFromEmitter seed emitter =
    Loop.for emitter.density (tupleSeedEmitter seed emitter) ( seed, [] ) |> Tuple.second


createParticleFromEmitter : Seed -> Emitter -> Particle
createParticleFromEmitter seed emitter =
    { position = emitter.position
    , velocity = rotateVector (generateRandomNum seed emitter.spread) emitter.velocity

    --, velocity = rotateVector emitter.spread emitter.velocity
    , acceleration = { x = 0, y = 0, z = 0 }
    , color = { red = 1, green = 0.5, blue = 0.25, alpha = 1 }
    , size = 1
    , gravity = 0
    }


init : Flags -> ( Model, Cmd Msg )
init data =
    let
        _ =
            Debug.log "flags data:" data
    in
    -- ( Loading, parseData data )
    -- lookupData data
    -- ( Loading "Loading", Cmd.none )
    ( Success { frameRate = 0, count = 1, data = data }, Cmd.none )


emitterIdentity : Emitter
emitterIdentity =
    { id = "identity"
    , position = { x = 0, y = 0, z = 0 }
    , spread = 0
    , velocity = { x = 0, y = 0, z = 0 }
    , color = [ { red = 1, green = 0, blue = 0, alpha = 0.4 } ]
    , size = 0
    , density = 1
    }



-- UPDATE


subscriptions _ =
    onAnimationFrameDelta Frame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        frameRate =
            case msg of
                Frame v ->
                    v
    in
    case model of
        Failure err ->
            ( model, Cmd.none )

        Loading str ->
            ( model, Cmd.none )

        Success m ->
            let
                seed0 =
                    Random.initialSeed m.count

                _ =
                    Debug.log "newParticles" newParticles

                particlesGroup : Emitter -> List Particle
                particlesGroup emitter =
                    -- createParticleFromEmitter seed0 emitter
                    groupParticlesFromEmitter seed0 emitter

                -- [1] Add new particles based on emitters params
                addParticlesFromEmitters : List Emitter -> List Particle
                addParticlesFromEmitters emittersList =
                    List.concatMap particlesGroup emittersList

                addVector : Point -> Point -> Point
                addVector a b =
                    { x = a.x + b.x, y = a.y + b.y, z = a.z + b.z }

                moveParticle : Particle -> Particle
                moveParticle particle =
                    { particle | position = addVector particle.position particle.velocity }

                -- NEW DATA BEEN ADDED TO DATA FIELD
                newParticles : List Particle
                newParticles =
                    addParticlesFromEmitters m.data.emitters
                        ++ m.data.particles
                        -- movemos las particulas
                        |> map moveParticle

                newEmitters : List Emitter
                newEmitters =
                    m.data.emitters

                newFields : List Field
                newFields =
                    m.data.fields

                newData =
                    { particles = newParticles
                    , emitters = newEmitters
                    , fields = newFields
                    }
            in
            -- UPDATE DATA
            -- -----------
            -- List.map addparticles m.data.emitters m.data.
            -- move particles
            --remove unbound
            --redraw
            ( Success { m | frameRate = frameRate, count = m.count + 1, data = newData }, Cmd.none )



-- addparticles: List Emitter -> List Particle -> List Particle
-- addparticles emitters particles =
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
            (modelToShape model)

        {--
            ++ [ shapes
                    [ fill Color.darkBlue ]
                    [ circle ( 20, 20 ) 5 ]
               , shapes
                    [ fill Color.darkBlue ]
                    [ circle ( 80, 120 ) 25 ]
               ]
               --}
        , div [] [ text "this is it" ]
        ]
    }


modelToShape : Model -> List Renderable
modelToShape model =
    case model of
        Failure e ->
            [ shapes [] [] ]

        Loading s ->
            [ shapes [] [] ]

        Success m ->
            fieldShapes m.data.fields


convertToCircle : Field -> Shape
convertToCircle field =
    circle ( field.position.x, field.position.y ) ((field.size |> toFloat) / 2)


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
        [ convertToCircle field ]


convertParticleToShapes : Particle -> Renderable
convertParticleToShapes particle =
    shapes
        [ fill
            (Color.fromRgba
                (particle.color
                    |> head
                    |> Maybe.withDefault defaultColor
                )
            )
        ]
        [ convertToCircle particle ]


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
