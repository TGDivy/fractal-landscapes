{-# LANGUAGE RecordWildCards #-}
module Mountains (mountain, mountainReflection, ground, groundTree, groundTree2) where

import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions
import System.Random
import Data.Text.Unsafe

-- Twister takes a line, and raises the line's midpoint by a certain value provided and returns the 2 original line points, and the raised point as a triangle.

twister :: Line -> Float -> [Point]
twister ly@(Line a b) delta = [a, c, b]
  where
    (Line _ (Point x y)) = scaleLine scaleFactor ly
    c = Point x (y-delta)
    --Here, the if you change the randomR values to 0.5,0.5 to get the exact midpoint, but to create more organic pictures I had to add a little chaos.
    scaleFactor = inlinePerformIO $ getStdRandom (randomR (0.47,0.53))

-- This function is a fractal for creating Mountain type images, Given a horizontal line by recursively using the twister function on the subsequent lines,
-- it creates a mountain.

creator :: Line -> Float -> (Scale,Scale) -> Colour -> BleachFactor -> Iterations -> Picture
creator ly height (heightFactLeft, heightFactRight) color bleachFact iter
  | iter<=0 = []
  | otherwise
    = (creator newLineLeft scaledHeightL (heightFactLeft, heightFactRight) bleachedColor bleachFact (iter-1)) ++
      [(Polygon (twister ly height) color Solid SolidFill)]              ++
      (creator newLineRight scaledHeightR (heightFactLeft, heightFactRight) bleachedColor bleachFact (iter-1))
      where
        (a:c:b:[])    = twister ly height --decomposing the triangle into points
        --selects the two new lines generated and applies twister.
        newLineLeft   = Line a c
        newLineRight  = Line c b
        --additional parameters used for complex images. (so that the mountains generated are asymmteric)

        scaledHeightL = (height * heightFactLeft             )
        scaledHeightR = (height * heightFactRight * direction)
        bleachedColor = bleach color bleachFact
        -- makes sure that there are steep downslops as well.
        direction     = (-1) ^ iter


-- These are test inputs to play with different mountains generated.

inLine            = Line (Point 0 480) (Point (1920) 480)
inHeight          = 200
inHeightFactLeft  = 0.7
inHeightFactRight = 0.75
inColor           = Colour 38 19 5 190
inColorReflection = Colour 38 19 5 60
inBleachFact      = (18,11,2,(-8))
inBleachReflection= (18,11,2,(0))

-- These are additional input variances for the base of the stumps as demonstrated.
inLineB            = Line (Point 0 (1080)) (Point 1920 (1080))
inHeightB          = 30
inHeightFactLeftB  = 0.8
inHeightFactRightB = 0.65
inColorB           = Colour 28 116 85 240
inBleachFactB      = (16,4,8,(-8))

-- Positioning them under the tree generate by pythogorusTree
inLineTree         = Line (Point 1430 (1080-180)) (Point 1640 (1080-180))
inLineTree2        = Line (Point 50 (1080-300)) (Point 260 (1080-300))

-- The outputs generated here.
mountain           =  creator inLine   inHeight  (inHeightFactLeft,inHeightFactRight) inColor           inBleachFact           6
mountainReflection =  creator inLine (-inHeight) (inHeightFactLeft,inHeightFactRight) inColorReflection inBleachReflection     6

ground             =  creator inLineB      (inHeightB)    (inHeightFactLeftB,inHeightFactRightB) inColorB inBleachFactB 8
groundTree         =  creator inLineTree   (inHeightB-15) (inHeightFactLeftB,inHeightFactRightB) inColorB inBleachFactB 6
groundTree2        =  creator inLineTree2  (inHeightB-15) (inHeightFactLeftB,inHeightFactRightB) inColorB inBleachFactB 6

-- Displaying the output.
picture = drawPicture 3 $ ground ++ groundTree ++ groundTree2
file = writePng "test.png" $ picture
