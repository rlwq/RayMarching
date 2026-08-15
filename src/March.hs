{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module March (
    MarchConfig (..),
) where

import Ray
import Vec
import Shape
import Scene

data MarchConfig = MarchConfig
    { epsilon :: Scalar
    , maxDist :: Scalar
    , maxSteps :: Int
    }

data Marched a = Marched
    { ray :: a
    , traveled :: !Scalar
    , minDist :: !Scalar
    , nearest :: Maybe Shape
    , steps :: !Int
    }

instance Semigroup (Marched a) where
    (<>) :: Marched a -> Marched a -> Marched a
    (<>) m1@(Marched _ _ d1 _ _) m2@(Marched _ _ d2 _ _) = if d1 <= d2 then m1 else m2

instance Monoid (Marched Ray) where
    mempty :: Marched Ray
    mempty = Marched (Ray (1/0) 0) (1/0) (1/0) Nothing 0

instance Functor Marched where
    fmap :: (a -> b) -> Marched a -> Marched b
    fmap f m = m {ray = f $ ray m}

instance Applicative Marched where
    pure :: a -> Marched a
    pure a = Marched a 0 (1/0) Nothing 0
    (<*>) :: Marched (a -> b) -> Marched a -> Marched b
    (<*>) f a = do
        f' <- f
        a' <- a
        pure (f' a')

instance Monad Marched where
    (>>=) :: Marched a -> (a -> Marched b) -> Marched b
    (>>=) (Marched x1 t1 d1 o1 s1) b = Marched x2 (t1 + t2) (min d1 d2) (if d1 <= d2 then o1 else o2) (s1 + s2)
        where (Marched x2 t2 d2 o2 s2) = b x1

stepToShape :: Shape -> Ray -> Marched Ray
stepToShape o@(Shape f _) r@(Ray x _) = Marched (advance d r) d d (Just o) 1
    where d = f x

stepInScene :: Scene -> Ray -> Marched Ray
stepInScene (Scene shapes _) r = mconcat $ map (\s -> stepToShape s r) shapes

marchInScene :: MarchConfig -> Scene -> Ray -> Marched Ray
marchInScene config scene ray = until stop (>>= (stepInScene scene)) (pure ray) 
    where stop (Marched _ t d _ s)
             | t >= maxDist config = True
             | d <= epsilon config = True
             | s >= maxSteps config = True
             | otherwise = False
