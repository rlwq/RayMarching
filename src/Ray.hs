module Ray (
    Ray (..),
) where

import Vec

data Ray = Ray
    { position :: Vec3 Scalar
    , direction :: Vec3 Scalar
    }
    deriving (Show, Eq)
