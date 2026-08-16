module Main where

import Camera
import Image
import March
import Render
import Scene
import Sdf
import Shape
import Skybox
import Textures
import Vec

main :: IO ()
main = do
    writeFile path (ppm (rasters scene (capture config resolution scene camera)))
    putStrLn ("wrote " ++ path)
  where
    path = "out.ppm"

resolution :: Resolution
resolution = (960, 720)

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
        { shapes =
            [ ground (-1) # checker white black
            , sphere (Vec3 (-0.35) 0 5.8) 1 # solid red
            , box (Vec3 0.75 (-0.4) 5.6) (Vec3 1.2 1.2 1.2) # solid blue
            ]
        , skybox = sky lightBlue skyBlue
        }

black, white, red, blue, lightBlue, skyBlue :: Color
black = Vec3 0.2 0.2 0.22
white = Vec3 0.9 0.9 0.85
red = Vec3 0.85 0.25 0.2
blue = Vec3 0.2 0.45 0.85
lightBlue = Vec3 0.75 0.8 0.9
skyBlue = Vec3 0.25 0.45 0.8
