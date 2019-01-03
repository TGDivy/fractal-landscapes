{-# LANGUAGE RecordWildCards #-}

module Grass (flowers) where

import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions
import PythogorusTree
import LsystemTree
import System.Random
import Data.Text.Unsafe

-- This functions takes a horizontal line. From the line's midpoint it generates a L-system tree. Between the new L-tree and the other two points,
-- it generates two more L-system base trees, reursively...

grass :: Line -> Float -> Angle -> (Scale, Scale) -> (Colour, Colour) -> (BleachFactor, BleachFactor) -> (Float, Float) -> Iterations -> Iterations -> Picture
grass ly@(Line a b) stemHeight angle (leftBranchScale, rightBranchScale) (colorL@(Colour rl gl bl ol), colorR@(Colour rr gr br oR)) (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) iter2 iter
  | iter<=0 = []
  | otherwise
     =(grass (Line a mid) (inlinePerformIO stemHeight') (inlinePerformIO angle') (inlinePerformIO leftBranchScale', inlinePerformIO rightBranchScale') (colorL', colorR') (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) (iter2) (iter-1)) ++
      (lSystemTree midLine (angle) (leftBranchScale, rightBranchScale) (colorL, colorR) (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) iter2) ++
      (grass (Line mid b) (inlinePerformIO stemHeight') (inlinePerformIO angle') (inlinePerformIO leftBranchScale', inlinePerformIO rightBranchScale') (colorL', colorR') (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) (iter2) (iter-1))
       where
        (Line _ mid@(Point xmid ymid)) = scaleLine scaleFactor ly
        midLine = Line (Point (xmid) (ymid)) (Point (xmid) (ymid-stemHeight))

        -- Some additional parameters, which based on the random function make the plants produced look organic.
        -- Note, the unwrapping of the IO takes place when the function is called, as we want different random value for both left and right branch of the tree.
        scaleFactor = inlinePerformIO $ getStdRandom (randomR (0.3,0.7))
        angle'              =  getStdRandom (randomR (pi*0.05 , pi*0.10))
        leftBranchScale'    =  getStdRandom (randomR (0.7, 0.9))
        rightBranchScale'   =  getStdRandom (randomR (0.7, 0.9))
        stemHeight'         =  getStdRandom (randomR (60 , 100))
        colorL'             =  Colour rl gl bl (ol-100)
        colorR'             =  Colour rr gr br (oR-100)
        -- angleScaleBrLeft'  = getStdRandom (randomR (,))
        -- angleScaleBrRight' = getStdRandom (randomR (,))

-- Inputs for the function to generate multiple L-trees
inLine              = Line (Point 0 (1080+90)) (Point 1920 (1080+90))
inAngle             = pi*0.15
inLeftBranchScale   = 0.8
inRightBranchScale  = 0.6
inColorL            = Colour 82 0 55 255
inColorR            = Colour 82 0 55 150
inbleachFactL       = (-3,0,-2,-8)
inbleachFactR       = (3,0,2,0)
inAngleScaleBrLeft  = 0.8
inAngleScaleBrRight = 1.1

-- Output
flowers = grass inLine 90 inAngle (inLeftBranchScale,inRightBranchScale) (inColorL,inColorR) (inbleachFactL,inbleachFactR) (inAngleScaleBrLeft,inAngleScaleBrRight) 7 5

-- Displaying the output to a file stream.
picture = drawPicture 3 flowers
file = writePng "Test1.png" $ picture
