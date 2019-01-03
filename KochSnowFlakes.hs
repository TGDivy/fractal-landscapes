{-# LANGUAGE RecordWildCards #-}

module KochSnowFlakes(snowFlakes
) where

import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions
import System.Random
import Data.Text.Unsafe


-- Use t create a Line which is based on the recursion pattern of KochSnowflakes
kochLine :: Int -> Point -> Point -> [Point]
kochLine n pS pE
  | n <= 0 = []
  | otherwise
  = [pS] ++ kochLine (n - 1) pS p1
         ++ kochLine (n - 1) p1 p2
         ++ kochLine (n - 1) p2 p3
         ++ kochLine (n - 1) p3 pE
         ++ [pE]
  where
    l1@(Line _ p1) = scaleLine (1 / 3) (Line pS pE)
    l2@(Line _ p3) = connectLine l1 l1
    (Line _ p2)    = rotateLine l2 (5 / 3 * pi)

-- This combines multiple KochSnowflake based lines, by forming a polygon out of them.
kochFlake :: Int -> Line -> [Point]
kochFlake n line
  | n<= 0 = []
  | otherwise = kochLine n p1 p2 ++ kochLine n p2 p3 ++ kochLine n p3 p1
   where
     [p1, p2, p3, _] = polygon 3 line

--Function that combines multiple KochFlakes into a picture

flakes :: Int -> Colour -> BleachFactor -> Picture
flakes n color bleachFact| n<=0 = []
         | otherwise = [Polygon (kochFlake 7 (Line a b)) (bleach color bleachFact) Solid SolidFill] ++
                        flakes (n-1) color bleachFact
         where
           midx = inlinePerformIO $ getStdRandom (randomR (20, 1900))
           midy = inlinePerformIO $ getStdRandom (randomR (20, 1060))
           size = inlinePerformIO $ getStdRandom (randomR (5, 8))
           orientaion = inlinePerformIO $ getStdRandom (randomR (0, 10))
           a    = Point (midx - size) (midy+orientaion)
           b    = Point (midx + size) (midy)



--These are values used for testing
x = kochFlake 5 (Line (Point 765 540) (Point 1250 540))
y = Polygon x blue Solid SolidFill

snowFlakes = flakes 30 inColour inBleachFact

inColour = Colour 85 215 170 200
inBleachFact = (7, 2, 3, 0)

-- To display the output
picture = drawPicture 1 [y]
file = writePng "snowFlake.png" $ picture
