module Scene (
    Scene (..),
    distanceTo,
) where

import Shape
import Vec

data Scene = Scene
    { shapes :: [Shape]
    , worldMaterial :: Vec3
    }

-- | Distance to the closest surface in the scene; infinite if it is empty.
distanceTo :: Scene -> Vec3 -> Scalar
distanceTo scene p = minimum (1 / 0 : map (`sdf` p) (shapes scene))
