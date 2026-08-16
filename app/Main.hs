module Main where

import Camera
import Image
import March
import Render
import Scene
import Shape
import Vec

main :: IO ()
main = do
    writeFile path (ppm (rasters scene (capture config resolution scene camera)))
    putStrLn ("wrote " ++ path)
  where
    path = "out.ppm"

resolution :: Resolution
resolution = (1920, 1440)

config :: MarchConfig
config =
    MarchConfig
        { hitEpsilon = 1e-4
        , maxTravel = 60
        , maxSteps = 240
        }

camera :: Camera
camera =
    Camera
        { position = Vec3 0 0.7 0
        , focal = 1.2
        , sensorW = 2
        , sensorH = 1.5
        }

scene :: Scene
scene =
    Scene
        { shapes = [ground, ball, cube]
        , background = sky
        }

ground :: Shape
ground =
    Shape
        { sdf = \(Vec3 _ y _) -> y + 1
        , material = checker
        }

ball :: Shape
ball =
    Shape
        { sdf = \point -> distance point (Vec3 (-1.1) 0 6) - 1
        , material = const (Vec3 0.85 0.25 0.2)
        }

cube :: Shape
cube =
    Shape
        { sdf = box (Vec3 1.3 (-0.4) 5.5) (Vec3 0.6 0.6 0.6)
        , material = const (Vec3 0.2 0.45 0.85)
        }

box :: Point -> Vec3 -> Point -> Scalar
box center halfExtents point = norm (vmax q 0) + min (vfold max q) 0
  where
    q = abs (point - center) - halfExtents

checker :: Point -> Color
checker (Vec3 x _ z) =
    if even (floor x + floor z :: Int)
        then Vec3 0.9 0.9 0.85
        else Vec3 0.2 0.2 0.22

sky :: Direction -> Color
sky (Vec3 _ y _) = horizon + max 0 y *^ (zenith - horizon)
  where
    horizon = Vec3 0.75 0.8 0.9
    zenith = Vec3 0.25 0.45 0.8
