module Image (
    ppm,
) where

import Vec

ppm :: Image -> String
ppm rows = unlines (header ++ map pixels rows)
  where
    header = ["P3", unwords [show width, show height], "255"]
    height = length rows
    width = case rows of
        [] -> 0
        (row : _) -> length row
    pixels = unwords . concatMap channels
    channels (Vec3 r g b) = map byte [r, g, b]
    byte :: Scalar -> String
    byte c = show (round (255 * min 1 (max 0 c)) :: Int)
