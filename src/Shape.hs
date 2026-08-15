module Shape (
    Shape (..),
) where

import Vec

data Shape = Shape
    { sdf :: Vec3 -> Scalar
    , material :: Vec3 -> Vec3
    }
