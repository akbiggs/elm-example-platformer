module Collisions exposing (Model, Effect(..), resolve)

import Player
import Ground
import Effects exposing (Effects)


-- MODEL


type alias Model =
    { player : Player.Model
    , ground : List Ground.Model
    }



-- UPDATE


type Effect
    = PlayerEffect Player.Effect
    | GroundEffect Ground.Effect


resolve : Model -> Effects Model Effect
resolve model =
    let
        groundCollidingWithPlayer =
            List.filter (isPlayerCollidingWithGround model.player) model.ground

        maybeHighestCollidingGroundTop =
            groundCollidingWithPlayer
                |> List.filter (isPlayerCollidingWithGround model.player)
                |> List.map Ground.top
                |> List.maximum
    in
        case maybeHighestCollidingGroundTop of
            Just highestCollidingGroundTop ->
                let
                    ( updatedPlayer, playerEffects ) =
                        Player.update (Player.LandOnGround { groundTop = highestCollidingGroundTop })
                            model.player
                in
                    Effects.init { model | player = updatedPlayer }
                        (List.map PlayerEffect playerEffects)

            Nothing ->
                Effects.return model


isPlayerCollidingWithGround : Player.Model -> Ground.Model -> Bool
isPlayerCollidingWithGround player ground =
    (Player.left player)
        <= ground.x
        && (Player.right player)
        >= ground.x
        && (Player.bottom player)
        <= (Ground.top ground)
