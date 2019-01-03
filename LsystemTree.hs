{-# LANGUAGE RecordWildCards #-}

module LsystemTree( lSystemTree, lTree
) where

import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions

-- When give a line and angle, it generates two new lines at the tip of the first line at an angle provided by the user.
branch :: Line -> Angle -> Scale -> Scale -> (Line,Line)
branch base angle leftBranchScale rightBranchScale
  = (r1,r2)
    where
      r1 = connectLine base $ scaleLine leftBranchScale $ rotateLine base ( angle)
      r2 = connectLine base $ scaleLine rightBranchScale $ rotateLine base (-angle)

-- It recursively applies branch (branching) to the new branches generated. The extra parameters are to manipulate how the branches appear.

lSystemTree :: Line -> Angle -> (Scale, Scale) -> (Colour, Colour) -> (BleachFactor, BleachFactor) -> (Float, Float) -> Iterations -> Picture
lSystemTree base@(Line p q) angle (leftBranchScale,rightBranchScale) (colorL, colorR) (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) iter
  | iter<=0   = [] --[Circle q 2 red Solid SolidFill] Uncomment this to add a circle at the tip of the final branch.
  | otherwise =
    (Path branchL (bleach colorL bleachL) Solid) : (Path branchR (bleach colorR bleachR) Solid) : [] ++  -- The code below might look complicated, but its just applying different parameters to the values
     (lSystemTree brLeft  (angle*angleScaleBrLeft)  (leftBranchScale, rightBranchScale) (colorL, colorR) (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) (iter-1)) ++
     (lSystemTree brRight (angle*angleScaleBrRight) (leftBranchScale, rightBranchScale) (colorL, colorR) (bleachL, bleachR) (angleScaleBrLeft, angleScaleBrRight) (iter-1))
        where
          (brRight@(Line p1 p2), brLeft@(Line q1 q2)) = branch base angle leftBranchScale rightBranchScale
          branchR, branchL :: [Point]
          branchR = [p1,p2]
          branchL = [q1,q2]

-- Different INput parameters
inLine              = Line (Point (960) (590)) (Point (960) (490))
inAngle             = pi*0.25
inLeftBranchScale   = 0.7
inRightBranchScale  = 0.8
inColorL            = Colour 130 22 188 255
inColorR            = Colour 25 60 209 255
inbleachFactL       = (10,2,0,-20)
inbleachFactR       = (10,2,0,-30)
inAngleScaleBrLeft  = 1
inAngleScaleBrRight = 1

-- The Picture of L-System-tree.
lTree = lSystemTree inLine inAngle (inLeftBranchScale, inRightBranchScale) (inColorL, inColorR) (inbleachFactL, inbleachFactR) (inAngleScaleBrLeft, inAngleScaleBrRight) 8

-- Displaying the output.
picture = drawPicture 3 lTree
file = writePng "LSystemtree.png" $ picture

-- This function was just an attemp to rotate L-systemfractals, and observe.
lSss :: Line -> Iterations -> Picture
lSss ly iter
| iter<=0 = []
| otherwise
= (lSystemTree rotatedL inAngle (inLeftBranchScale, inRightBranchScale) (inColorL, inColorR) (inbleachFactL, inbleachFactR) (inAngleScaleBrLeft, inAngleScaleBrRight) 8) ++ lSss rotatedL (iter-1)
where
  rotatedL = (rotateLine ly (pi/24))

  --lTree = lSystemTree inLine (0.2) inLeftBranchScale inRightBranchScale inColorL inColorR inbleachFactL inbleachFactR (1.35) (-1.1) 8

  sample = lSss inLine 2
