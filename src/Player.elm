module Player exposing (Model, Msg(..), MoveDirection(..), Effect(..), init, left, right, top, bottom, update, view)

-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Color
import Time exposing (Time)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Vector exposing (Vector)


-- MODEL


type alias Model =
    { position : Vector
    , velocity : Vector
    }


init : Effects Model Effect
init =
    ( { position = Vector.zero
      , velocity = Vector.zero
      }
    , []
    )


moveSpeed : Float
moveSpeed =
    0.2


jumpSpeed : Float
jumpSpeed =
    0.2


maxFallSpeed : Float
maxFallSpeed =
    0.2


width : Float
width =
    20


height : Float
height =
    20


gravity : Vector
gravity =
    Vector.init 0 -0.02


left : Model -> Float
left model =
    (Vector.getX model.position) - (width / 2)


right : Model -> Float
right model =
    (Vector.getX model.position) + (width / 2)


bottom : Model -> Float
bottom model =
    (Vector.getY model.position) - (height / 2)


top : Model -> Float
top model =
    (Vector.getY model.position) + (height / 2)



-- UPDATE


type MoveDirection
    = Left
    | Right


type Msg
    = Tick Time
    | Move MoveDirection
    | Jump
    | LandOnGround { groundTop : Float }


type Effect
    = PlaySound String
    | IncrementScore Int


update : Msg -> Model -> Effects Model Effect
update msg model =
    case msg of
        Tick dt ->
            Effects.return
                { model
                    | position = Vector.add model.position (Vector.mulS dt model.velocity)
                    , velocity =
                        Vector.add model.velocity gravity
                            |> (\( x, y ) ->
                                    ( x, max y (-maxFallSpeed) )
                               )
                            |> Vector.setX 0
                }

        Move dir ->
            case dir of
                Left ->
                    Effects.return { model | velocity = Vector.setX -moveSpeed model.velocity }

                Right ->
                    Effects.return { model | velocity = Vector.setX moveSpeed model.velocity }

        Jump ->
            Effects.init { model | velocity = Vector.setY jumpSpeed model.velocity }
                [ PlaySound "jump.wav" ]

        LandOnGround { groundTop } ->
            Effects.init
                { model
                    | position = Vector.setY (groundTop + (height / 2)) model.position
                    , velocity = Vector.setY 0 model.velocity
                }
                [ PlaySound "land.wav" ]



-- VIEW


view : Model -> Form
view model =
    Collage.rect width height
        |> Collage.filled Color.black
        |> Collage.move model.position
