module Scene (
    Scene (..),
) where

import Shape
import Vec

data Scene = Scene
    { shapes :: [Shape]
    , background :: Direction -> Color
    }

