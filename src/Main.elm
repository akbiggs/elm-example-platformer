module Main exposing (Model, Msg, init, update, view)

-- EXTERNAL IMPORTS

import Collage
import Element
import Text
import AnimationFrame
import Html exposing (Html)
import Html.App
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Update
import Player
import Bounds
import Ground
import Collisions


-- PROGRAM


main : Program Never
main =
    Html.App.program
        { init = init |> Effects.toCmd
        , update = \msg model -> update msg model |> Effects.toCmd
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { score : Int
    , player : Player.Model
    , ground : List Ground.Model
    , keyboard : Keyboard.Model
    }


init : Effects Model (Cmd Msg)
init =
    let
        ( player, playerEffects ) =
            Player.init

        ( ground, groundEffects ) =
            [ Ground.init { x = 0, height = 50 }
            , Ground.init { x = 1, height = 50 }
            , Ground.init { x = 2, height = 50 }
            , Ground.init { x = 3, height = 40 }
            , Ground.init { x = 4, height = 40 }
            , Ground.init { x = 5, height = 20 }
            , Ground.init { x = 6, height = 20 }
            , Ground.init { x = 7, height = 20 }
            , Ground.init { x = -1, height = 50 }
            , Ground.init { x = -2, height = 60 }
            , Ground.init { x = -3, height = 60 }
            ]
                |> Effects.batch

        ( keyboard, keyboardCmd ) =
            Keyboard.init
    in
        Effects.init
            { score = 0
            , player = player
            , ground = ground
            , keyboard = keyboard
            }
            [ Cmd.map KeyboardMsg keyboardCmd ]
            `Effects.andThen` Effects.handle handlePlayerEffect playerEffects
            `Effects.andThen` Effects.handle handleGroundEffect groundEffects



-- UPDATE


type Msg
    = Tick Time
    | IncrementScore Int
    | PlaySound String
    | KeyboardMsg Keyboard.Msg


update : Msg -> Model -> Effects Model (Cmd Msg)
update msg model =
    case msg of
        Tick dt ->
            let
                -- (Player, List Player.Effect)
                ( updatedPlayer, playerEffects ) =
                    Player.update (Player.Tick dt) model.player
                        `Effects.andThen` Update.runIf (Keyboard.isPressed Keyboard.Space model.keyboard)
                                            (Player.update Player.Jump)
                        `Effects.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowLeft model.keyboard)
                                            (Player.update (Player.Move Player.Left))
                        `Effects.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowRight model.keyboard)
                                            (Player.update (Player.Move Player.Right))
            in
                Effects.return { model | player = updatedPlayer }
                    `Effects.andThen` Effects.handle handlePlayerEffect playerEffects
                    `Effects.andThen` handleCollisions

        KeyboardMsg keyMsg ->
            let
                ( updatedKeyboard, keyboardCmd ) =
                    Keyboard.update keyMsg model.keyboard
            in
                Effects.init { model | keyboard = updatedKeyboard }
                    [ Cmd.map KeyboardMsg keyboardCmd ]

        IncrementScore amount ->
            Effects.return { model | score = model.score + amount }

        PlaySound filename ->
            -- TODO
            Effects.return model


handleCollisions : Model -> Effects Model (Cmd Msg)
handleCollisions model =
    let
        collisionsModel =
            { player = model.player
            , ground = model.ground
            }

        ( updatedCollisionsModel, collisionsEffects ) =
            Collisions.resolve collisionsModel
    in
        Effects.return
            { model
                | player = updatedCollisionsModel.player
                , ground = updatedCollisionsModel.ground
            }
            `Effects.andThen` Effects.handle handleCollisionEffect collisionsEffects


handlePlayerEffect : Effects.Handler Player.Effect Model (Cmd Msg)
handlePlayerEffect effect model =
    case effect of
        Player.PlaySound filename ->
            update (PlaySound filename) model

        Player.IncrementScore amount ->
            update (IncrementScore amount) model


handleGroundEffect : Effects.Handler Ground.Effect Model (Cmd Msg)
handleGroundEffect =
    Effects.ignoreUnused


handleCollisionEffect : Effects.Handler Collisions.Effect Model (Cmd Msg)
handleCollisionEffect effect model =
    case effect of
        Collisions.PlayerEffect playerEffect ->
            handlePlayerEffect playerEffect model

        Collisions.GroundEffect groundEffect ->
            handleGroundEffect groundEffect model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Sub.map KeyboardMsg Keyboard.subscriptions
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        scene =
            List.concat
                [ [ Player.view model.player ]
                , List.map Ground.view model.ground
                , [ Collage.text (Text.fromString (toString model.score)) ]
                ]
    in
        Collage.collage (round Bounds.width)
            (round Bounds.height)
            scene
            |> Element.toHtml
