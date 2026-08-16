module Sdf (
    Sdf,
    sphere,
    box,
    ground,
) where

import Vec

type Sdf = Point -> Scalar

sphere :: Point -> Scalar -> Sdf
sphere center radius point = distance point center - radius

box :: Point -> Vec3 -> Sdf
box center dimensions point = norm (vmax q 0) + min (vfold max q) 0
  where
    q = abs (point - center) - 0.5 *^ dimensions

ground :: Scalar -> Sdf
ground level (Vec3 _ y _) = y - level
