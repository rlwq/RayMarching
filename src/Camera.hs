module Camera (
    Camera (..),
    sensorRay,
) where

import Ray (Ray (Ray))
import Vec

data Camera = Camera
    { position :: Vec3
    , focal :: Scalar
    , sensorW :: Scalar
    , sensorH :: Scalar
    }
    deriving (Show, Eq)

sensorRay :: Camera -> Vec2 -> Ray
sensorRay (Camera p f w h) (Vec2 u v) = Ray (p + offset) (normalize offset)
  where
    offset = Vec3 (u * w / 2) (v * h / 2) f
