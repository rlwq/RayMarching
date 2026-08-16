module Shape (
    Shape (..),
    (#),
) where

import Sdf
import Textures

data Shape = Shape
    { sdf :: Sdf
    , material :: Material
    }

infixl 1 #

(#) :: Sdf -> Material -> Shape
(#) = Shape
