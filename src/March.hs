{-# LANGUAGE InstanceSigs #-}

module March (
    MarchConfig (..),
    MarchResult (..),
    march,
) where

import Ray
import Scene
import Shape
import Vec

data MarchConfig = MarchConfig
    { hitEpsilon :: Scalar
    , maxTravel :: Scalar
    , maxSteps :: Int
    }

data MarchResult
    = Hit Ray Shape
    | Miss Direction

data Marched a = Marched
    { value :: a
    , traveled :: !Scalar
    , minDist :: !Scalar
    , nearest :: Maybe Shape
    , steps :: !Int
    }

instance Semigroup (Marched a) where
    (<>) :: Marched a -> Marched a -> Marched a
    (<>) left right = if minDist left <= minDist right then left else right

instance Functor Marched where
    fmap :: (a -> b) -> Marched a -> Marched b
    fmap f marched = marched {value = f (value marched)}

instance Applicative Marched where
    pure :: a -> Marched a
    pure a = Marched a 0 (1 / 0) Nothing 0

    (<*>) :: Marched (a -> b) -> Marched a -> Marched b
    (<*>) f a = do
        f' <- f
        a' <- a
        pure (f' a')

instance Monad Marched where
    (>>=) :: Marched a -> (a -> Marched b) -> Marched b
    (>>=) before after =
        Marched
            { value = value next
            , traveled = traveled before + traveled next
            , minDist = min (minDist before) (minDist next)
            , nearest = if minDist before <= minDist next then nearest before else nearest next
            , steps = steps before + steps next
            }
      where
        next = after (value before)

stepToShape :: Shape -> Ray -> Marched Ray
stepToShape shape ray = Marched (advance dist ray) dist dist (Just shape) 1
  where
    dist = sdf shape (origin ray)

stepInScene :: Scene -> Ray -> Marched Ray
stepInScene scene ray = case map (`stepToShape` ray) (shapes scene) of
    [] -> Marched ray (1 / 0) (1 / 0) Nothing 0
    (step : rest) -> foldl' (<>) step rest

marchInScene :: MarchConfig -> Scene -> Ray -> Marched Ray
marchInScene config scene ray = until stop (>>= stepInScene scene) (pure ray)
  where
    stop marched =
        traveled marched >= maxTravel config
            || minDist marched <= hitEpsilon config
            || steps marched >= maxSteps config

marchResult :: MarchConfig -> Marched Ray -> MarchResult
marchResult config marched
    | minDist marched > hitEpsilon config = missed
    | otherwise = maybe missed (Hit (value marched)) (nearest marched)
  where
    missed = Miss (direction (value marched))

march :: MarchConfig -> Scene -> Ray -> MarchResult
march config scene ray = marchResult config (marchInScene config scene ray)
