{-# LANGUAGE InstanceSigs #-}

module Vec (
    Scalar,
    Point,
    Direction,
    Color,
    Image,
    Vec2 (..),
    Vec3 (..),
    Vector (..),
    (*^),
    dot,
    cross,
    norm,
    normSq,
    normalize,
    distance,
    vmin,
    vmax,
) where

type Scalar = Float

type Point = Vec3

type Direction = Vec3

type Color = Vec3

type Image = [[Color]]

data Vec2 = Vec2 !Scalar !Scalar
    deriving (Show, Eq)

data Vec3 = Vec3 !Scalar !Scalar !Scalar
    deriving (Show, Eq)

class Vector v where
    vmap :: (Scalar -> Scalar) -> v -> v
    vzip :: (Scalar -> Scalar -> Scalar) -> v -> v -> v
    vfold :: (Scalar -> Scalar -> Scalar) -> v -> Scalar

instance Vector Vec2 where
    vmap :: (Scalar -> Scalar) -> Vec2 -> Vec2
    vmap f (Vec2 x y) = Vec2 (f x) (f y)

    vzip :: (Scalar -> Scalar -> Scalar) -> Vec2 -> Vec2 -> Vec2
    vzip f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)

    vfold :: (Scalar -> Scalar -> Scalar) -> Vec2 -> Scalar
    vfold f (Vec2 x y) = f x y

instance Vector Vec3 where
    vmap :: (Scalar -> Scalar) -> Vec3 -> Vec3
    vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

    vzip :: (Scalar -> Scalar -> Scalar) -> Vec3 -> Vec3 -> Vec3
    vzip f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)

    vfold :: (Scalar -> Scalar -> Scalar) -> Vec3 -> Scalar
    vfold f (Vec3 x y z) = f x (f y z)

instance Num Vec2 where
    (+) :: Vec2 -> Vec2 -> Vec2
    (+) = vzip (+)

    (*) :: Vec2 -> Vec2 -> Vec2
    (*) = vzip (*)

    abs :: Vec2 -> Vec2
    abs = vmap abs

    signum :: Vec2 -> Vec2
    signum = vmap signum

    fromInteger :: Integer -> Vec2
    fromInteger n = Vec2 s s
      where
        s = fromInteger n

    negate :: Vec2 -> Vec2
    negate = vmap negate

instance Num Vec3 where
    (+) :: Vec3 -> Vec3 -> Vec3
    (+) = vzip (+)

    (*) :: Vec3 -> Vec3 -> Vec3
    (*) = vzip (*)

    abs :: Vec3 -> Vec3
    abs = vmap abs

    signum :: Vec3 -> Vec3
    signum = vmap signum

    fromInteger :: Integer -> Vec3
    fromInteger n = Vec3 s s s
      where
        s = fromInteger n

    negate :: Vec3 -> Vec3
    negate = vmap negate

instance Fractional Vec2 where
    (/) :: Vec2 -> Vec2 -> Vec2
    (/) = vzip (/)

    fromRational :: Rational -> Vec2
    fromRational r = Vec2 s s
      where
        s = fromRational r

instance Fractional Vec3 where
    (/) :: Vec3 -> Vec3 -> Vec3
    (/) = vzip (/)

    fromRational :: Rational -> Vec3
    fromRational r = Vec3 s s s
      where
        s = fromRational r

infixl 7 *^

(*^) :: (Vector v) => Scalar -> v -> v
(*^) s = vmap (s *)

dot :: (Vector v) => v -> v -> Scalar
dot u v = vfold (+) (vzip (*) u v)

normSq :: (Vector v) => v -> Scalar
normSq v = dot v v

norm :: (Vector v) => v -> Scalar
norm = sqrt . normSq

normalize :: (Vector v) => v -> v
normalize v = vmap (/ norm v) v

distance :: (Vector v) => v -> v -> Scalar
distance u v = norm (vzip (-) u v)

vmin :: (Vector v) => v -> v -> v
vmin = vzip min

vmax :: (Vector v) => v -> v -> v
vmax = vzip max

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
