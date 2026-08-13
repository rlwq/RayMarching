module Transform (
    translate,
) where

import Vec

translate :: Vec3 Scalar -> Vec3 Scalar -> Vec3 Scalar
translate offset value = value - offset
