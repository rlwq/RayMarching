module Skybox (
    Skybox,
    sky,
) where

import Vec

type Skybox = Direction -> Color

sky :: Color -> Color -> Skybox
sky horizon zenith (Vec3 _ y _) = horizon + max 0 y *^ (zenith - horizon)
