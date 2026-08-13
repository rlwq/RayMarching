{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Vec (
    Scalar,
    Vec2 (..),
    Vec3 (..),
    (*^),
    dot,
    cross,
    mag,
    magSq,
    normalize,
    distance,
    vmin,
    vmax,
) where

type Scalar = Float

data Vec2 a = Vec2 a a
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Vec3 a = Vec3 a a a
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Vec2 where
    pure :: a -> Vec2 a
    pure a = Vec2 a a

    (<*>) :: Vec2 (a -> b) -> Vec2 a -> Vec2 b
    Vec2 f g <*> Vec2 a b = Vec2 (f a) (g b)

instance Applicative Vec3 where
    pure :: a -> Vec3 a
    pure a = Vec3 a a a

    (<*>) :: Vec3 (a -> b) -> Vec3 a -> Vec3 b
    Vec3 f g h <*> Vec3 a b c = Vec3 (f a) (g b) (h c)

instance (Num a) => Num (Vec2 a) where
    (+) :: Vec2 a -> Vec2 a -> Vec2 a
    (+) = liftA2 (+)

    (*) :: Vec2 a -> Vec2 a -> Vec2 a
    (*) = liftA2 (*)

    abs :: Vec2 a -> Vec2 a
    abs = fmap abs

    signum :: Vec2 a -> Vec2 a
    signum = fmap signum

    fromInteger :: Integer -> Vec2 a
    fromInteger = pure . fromInteger

    negate :: Vec2 a -> Vec2 a
    negate = fmap negate

instance (Num a) => Num (Vec3 a) where
    (+) :: Vec3 a -> Vec3 a -> Vec3 a
    (+) = liftA2 (+)

    (*) :: Vec3 a -> Vec3 a -> Vec3 a
    (*) = liftA2 (*)

    abs :: Vec3 a -> Vec3 a
    abs = fmap abs

    signum :: Vec3 a -> Vec3 a
    signum = fmap signum

    fromInteger :: Integer -> Vec3 a
    fromInteger = pure . fromInteger

    negate :: Vec3 a -> Vec3 a
    negate = fmap negate

instance (Fractional a) => Fractional (Vec2 a) where
    (/) :: Vec2 a -> Vec2 a -> Vec2 a
    (/) = liftA2 (/)

    fromRational :: Rational -> Vec2 a
    fromRational = pure . fromRational

instance (Fractional a) => Fractional (Vec3 a) where
    (/) :: Vec3 a -> Vec3 a -> Vec3 a
    (/) = liftA2 (/)

    fromRational :: Rational -> Vec3 a
    fromRational = pure . fromRational

infixl 7 *^

(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) s = fmap (s *)

dot :: (Applicative f, Foldable f, Num a) => f a -> f a -> a
dot u v = sum (liftA2 (*) u v)

magSq :: (Applicative f, Foldable f, Num a) => f a -> a
magSq v = dot v v

mag :: (Applicative f, Foldable f, Floating a) => f a -> a
mag = sqrt . magSq

normalize :: (Applicative f, Foldable f, Floating a) => f a -> f a
normalize v = fmap (/ mag v) v

distance :: (Applicative f, Foldable f, Floating a) => f a -> f a -> a
distance u v = mag (liftA2 (-) u v)

vmin :: (Applicative f, Ord a) => f a -> f a -> f a
vmin = liftA2 min

vmax :: (Applicative f, Ord a) => f a -> f a -> f a
vmax = liftA2 max

cross :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
