module Render (
    capture,
    raster,
    rasters,
) where

import Camera (Camera, Resolution, sensorRays)
import March (MarchConfig, MarchResult (..), march)
import Ray (origin)
import Scene (Scene, skybox)
import Shape (material)
import Vec (Color, Image)

capture :: MarchConfig -> Resolution -> Scene -> Camera -> [[MarchResult]]
capture config resolution scene camera =
    (fmap . fmap) (march config scene) (sensorRays resolution camera)

raster :: Scene -> MarchResult -> Color
raster _ (Hit ray shape) = material shape (origin ray)
raster scene (Miss escaped) = skybox scene escaped

rasters :: Scene -> [[MarchResult]] -> Image
rasters scene = (fmap . fmap) (raster scene)
