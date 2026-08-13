{-# LANGUAGE InstanceSigs #-}

module March (
    MarchConfig (..),
    MarchInfo (..),
    stepInfo,
    march,
    marchInSDF,
    raymarch,
) where

import Data.Monoid (Sum (..))
import Data.Semigroup (Min (..))
import Ray
import SDF
import Vec

data MarchInfo = MarchInfo
    { traveled :: Sum Scalar
    , minStep :: Min Scalar
    , steps :: Sum Int
    }
    deriving (Show, Eq)

instance Semigroup MarchInfo where
    (<>) :: MarchInfo -> MarchInfo -> MarchInfo
    MarchInfo t1 m1 s1 <> MarchInfo t2 m2 s2 =
        MarchInfo (t1 <> t2) (m1 <> m2) (s1 <> s2)

instance Monoid MarchInfo where
    mempty :: MarchInfo
    mempty = MarchInfo mempty (Min (1 / 0)) mempty

stepInfo :: Scalar -> MarchInfo
stepInfo d = MarchInfo (Sum d) (Min d) (Sum 1)

data MarchConfig = MarchConfig
    { epsilon :: Scalar
    , maxDist :: Scalar
    , maxSteps :: Int
    }
    deriving (Show, Eq)

march :: Scalar -> Ray -> (MarchInfo, Ray)
march d (Ray p dir) = (stepInfo d, Ray (p + d *^ dir) dir)

marchInSDF :: SDF (Vec3 Scalar) -> Ray -> (MarchInfo, Ray)
marchInSDF sdf ray@(Ray p _) = march (function sdf p) ray

raymarch :: MarchConfig -> SDF (Vec3 Scalar) -> Ray -> (MarchInfo, Ray)
raymarch (MarchConfig eps maxD maxS) sdf ray =
    until (stop . fst) (>>= marchInSDF sdf) (pure ray)
  where
    stop (MarchInfo t m s) =
        getSum s >= maxS || getSum t > maxD || getMin m < eps
