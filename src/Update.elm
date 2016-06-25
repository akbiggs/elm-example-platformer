module Update exposing (runIf)

import Effects exposing (Effects)


runIf : Bool -> (model -> Effects model effect) -> model -> Effects model effect
runIf pred update model =
    if pred then
        update model
    else
        Effects.return model
