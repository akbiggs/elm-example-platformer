module Ground exposing (Model, Effect, init, top, view)

-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Color
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Bounds


-- MODEL


type alias Model =
    { x : Float
    , height : Float
    }


type alias InitArgs =
    { x : Float
    , height : Float
    }



-- Ground.init { x = 100, height = 200 }


init : InitArgs -> Effects Model Effect
init { x, height } =
    Effects.return
        { x = x
        , height = height
        }


top : Model -> Float
top model =
    model.height - (Bounds.height / 2)


y : Model -> Float
y model =
    (top model) - (model.height / 2)



-- UPDATE


type alias Effect =
    Effects.None



-- ()
-- VIEW


view : Model -> Form
view model =
    Collage.rect 1 model.height
        |> Collage.filled Color.green
        |> Collage.move ( model.x, y model )
