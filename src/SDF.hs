{-# LANGUAGE InstanceSigs #-}

module SDF (
    SDF (..),
    sphere,
) where

import Data.Functor.Contravariant
import Vec

newtype SDF a = SDF {function :: a -> Scalar}

instance Contravariant SDF where
    contramap :: (a' -> a) -> SDF a -> SDF a'
    contramap f (SDF g) = SDF (g . f)

sphere :: Scalar -> SDF (Vec3 Scalar)
sphere r = SDF (\p -> mag p - r)
