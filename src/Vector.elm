module Vector exposing (Vector, init, zero, length, normalize, getX, getY, setX, setY, add, sub, mul, div, mulS, divS, dot, cross, rotate)


type alias Vector =
    ( Float, Float )


init : Float -> Float -> Vector
init x y =
    ( x, y )


zero : Vector
zero =
    ( 0, 0 )


length : Vector -> Float
length vector =
    let
        ( x, y ) =
            vector
    in
        sqrt (x ^ 2 + y ^ 2)


normalize : Vector -> Vector
normalize vector =
    divS (length vector) vector


getX : Vector -> Float
getX ( x, _ ) =
    x


getY : Vector -> Float
getY ( _, y ) =
    y


setX : Float -> Vector -> Vector
setX x ( _, y ) =
    init x y


setY : Float -> Vector -> Vector
setY y ( x, _ ) =
    init x y


binOp : (Float -> Float -> Float) -> Vector -> Vector -> Vector
binOp op x y =
    let
        ( xx, xy ) =
            x

        ( yx, yy ) =
            y
    in
        ( xx `op` yx, xy `op` yy )


add : Vector -> Vector -> Vector
add =
    binOp (+)


sub : Vector -> Vector -> Vector
sub =
    binOp (-)


mul : Vector -> Vector -> Vector
mul =
    binOp (*)


div : Vector -> Vector -> Vector
div =
    binOp (/)


binOpS : (Float -> Float -> Float) -> Float -> Vector -> Vector
binOpS op scalar vector =
    let
        ( x, y ) =
            vector
    in
        ( x `op` scalar, y `op` scalar )


mulS : Float -> Vector -> Vector
mulS =
    binOpS (*)


divS : Float -> Vector -> Vector
divS =
    binOpS (/)


dot : Vector -> Vector -> Float
dot x y =
    let
        ( xx, xy ) =
            x

        ( yx, yy ) =
            y
    in
        xx * yx + xy * yy


cross : Vector -> Vector -> Float
cross x y =
    let
        ( xx, xy ) =
            x

        ( yx, yy ) =
            y
    in
        xx * yy - xy * yx


rotate : Float -> Vector -> Vector
rotate angle vector =
    let
        ( x, y ) =
            vector

        c =
            cos angle

        s =
            sin angle
    in
        ( x * c + y * s, y * c - x * s )
