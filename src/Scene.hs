module Scene (
    Scene (..),
) where

import Shape
import Skybox

data Scene = Scene
    { shapes :: [Shape]
    , skybox :: Skybox
    }
