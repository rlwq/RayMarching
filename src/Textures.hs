module Textures (
    Material,
    solid,
    checker,
) where

import Vec

type Material = Point -> Color

solid :: Color -> Material
solid = const

checker :: Color -> Color -> Material
checker light dark (Vec3 x _ z) =
    if even (floor x + floor z :: Int)
        then light
        else dark
