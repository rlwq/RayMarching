module Camera (
    Camera (..),
    Resolution,
    sensorRay,
    sensorRays,
) where

import Ray (Ray (Ray))
import Vec

data Camera = Camera
    { position :: Point
    , focal :: Scalar
    , sensorW :: Scalar
    , sensorH :: Scalar
    }
    deriving (Show, Eq)

type Resolution = (Int, Int)

sensorRay :: Camera -> Vec2 -> Ray
sensorRay camera (Vec2 u v) = Ray (position camera + offset) (normalize offset)
  where
    offset = Vec3 (u * sensorW camera / 2) (v * sensorH camera / 2) (focal camera)

sensorRays :: Resolution -> Camera -> [[Ray]]
sensorRays (width, height) camera =
    [ [sensorRay camera (Vec2 (center x width) (negate (center y height))) | x <- [0 .. width - 1]]
    | y <- [0 .. height - 1]
    ]
  where
    center :: Int -> Int -> Scalar
    center i n = 2 * (fromIntegral i + 0.5) / fromIntegral n - 1
