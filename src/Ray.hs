module Ray (
    Ray (..),
    advance,
) where

import Vec

data Ray = Ray
    { origin :: Point
    , direction :: Direction
    }
    deriving (Show, Eq)

advance :: Scalar -> Ray -> Ray
advance t ray = ray {origin = origin ray + t *^ direction ray}
