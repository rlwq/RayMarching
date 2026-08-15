module Ray (
    Ray (..),
    advance,
) where

import Vec

data Ray = Ray
    { position :: Vec3
    , direction :: Vec3
    }
    deriving (Show, Eq)

advance :: Float -> Ray -> Ray
advance t (Ray p d) = Ray (p + t *^ d) d
