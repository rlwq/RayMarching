module Shape (
    Shape (..),
) where

import Vec

data Shape = Shape
    { sdf :: Point -> Scalar
    , material :: Point -> Color
    }
