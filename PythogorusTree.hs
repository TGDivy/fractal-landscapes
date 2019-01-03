{-# LANGUAGE RecordWildCards #-}

module PythogorusTree(
  pythogorusTree, pTree, pTree2, pTree3
) where

import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions
import System.Random
import Data.Text.Unsafe

-- Takes in a line, based on the line it generates a square.
-- Based on the opposite side of the square, it generates a triangle
-- Based on the remaining two sides of the triangle, it generates two more squares, recursive....
-- The other parameters like angle, scale.. affect the way the triangles or rectangles appear (special case square)
pythogorusTree :: Line -> Angle -> Scale -> Scale -> Colour -> Colour -> BleachFactor -> BleachFactor -> Iterations -> Picture
pythogorusTree initialLine@(Line p q) angle rectscale triscale colorTri colorRec bleachfactRec bleachfactTri iter
  | iter <= 0 = [Circle q 1 (Colour 239 172 202 255) Solid SolidFill]
  | otherwise
    = sq:[triangle]
    ++ (pythogorusTree n1 angleL rectscaleL' triscaleL' bleachedColorTri bleachedColorRec bleachfactRec bleachfactTri (iter-1))
    ++ (pythogorusTree n2 angleR rectscaleR' triscaleR' bleachedColorTri bleachedColorRec bleachfactRec bleachfactTri (iter-1))
    where
      (sq, triLine)       = rectangle initialLine rectscale colorRec
      bleachedColorTri    = bleach colorTri bleachfactTri
      bleachedColorRec    = bleach colorRec bleachfactRec

      -- These are additional changes to the already provided parameters, these make the tree much more dynamic and organcic, Change these values to affect how each subsequent branch is formed
      iterF               = fromIntegral iter
      angleL              = inlinePerformIO $ getStdRandom (randomR (angle- pi/12  , angle+ pi/12))
      angleR              = inlinePerformIO $ getStdRandom (randomR (angle- pi/12  , angle+ pi/12))
      triscaleL'          = inlinePerformIO $ getStdRandom (randomR (triscale -iterF/300, triscale+iterF/300))
      triscaleR'          = inlinePerformIO $ getStdRandom (randomR (triscale -iterF/300, triscale+iterF/300))
      rectscaleL'         = inlinePerformIO $ getStdRandom (randomR (rectscale-iterF/120, rectscale+iterF/120))
      rectscaleR'         = inlinePerformIO $ getStdRandom (randomR (rectscale-iterF/120, rectscale+iterF/120))
      (triangle, n1, n2)  = triangles triLine angle triscale colorTri

-- Input values to play around with the display of the PythogorusTree

inLine          = Line (Point 1505 (1080-180)) (Point 1565 (1080-180))
inAngle         = pi*0.55
inScaleRect     = 3.5
inTriScale      = 0.55
inColorTri      = Colour 239 172 202 100
inColorRec      = Colour 51 13 6 255
inbleachFactRec = (4,3,2,-13)
inbleachFactTri = (1,2,1,10)

--Additional inputs to have more PythogorusTrees
inLine2          = Line (Point 125 (1080-300)) (Point 185 (1080-300))
inLine3          = Line (Point 1525 (1080-180)) (Point 1585 (1080-180))

inColorFadedT    = Colour 239 172 202 0
inColorFadedR    = Colour 51 13 6 140
fadedR           = (4,3,2,-8)
fadedT           = (1,2,1,10)
inLineFade       = Line (Point 1025 (1080-240)) (Point 1085 (1080-240))

-- The pictures of each PythogorusTree descirbed
pTree  = pythogorusTree inLine inAngle inScaleRect inTriScale inColorTri inColorRec inbleachFactRec  inbleachFactTri 14

pTree2 = pythogorusTree inLine2 (pi*0.5) 2.5 inTriScale inColorTri inColorRec inbleachFactRec  inbleachFactTri 14

pTree3 = pythogorusTree inLine3 inAngle inScaleRect inTriScale inColorFadedT inColorFadedR fadedR fadedT 14

-- Displaying the pictures.
picture = drawPicture 3 $  pTree
file = writePng "Ptrees.png" $ picture
