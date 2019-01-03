module GraphicsHelperFunctions(
  rotate90, rotate270, rotateLine,
  scaleLine, connectLine, midpoint,
  rectangle, triangles, polygon,
  Angle, Scale, Iterations, BleachFactor,
  bleach
) where

import Codec.Picture
import GraphicsM

type Angle = Float
type RectangleScale = Float
type Iterations = Int
type Scale = Float
type BleachFactor = (Int,Int,Int,Int)

  -- Function that changes the colors based on the values provided.
bleach :: Colour -> BleachFactor -> Colour
bleach (Colour r g b o) (r',g',b',o') = Colour (r+r') (g+g') (b+b') (o+o')

  -- Rotation based functions.
rotateLine :: Line -> Float -> Line
rotateLine ly@(Line (Point x1 y1) (Point x2 y2)) angle
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = x0 * cos angle - y0 * sin angle
    y' = x0 * sin angle + y0 * cos angle

rotate90 :: Line -> Point
rotate90 (Line (Point x1 y1) (Point x2 y2))
  = Point (-1 * ny + x1)
          (nx + y1)
  where
    nx = x2 - x1
    ny = y2 - y1

rotate270 :: Line -> Point
rotate270 (Line (Point x1 y1) (Point x2 y2))
  = Point (ny + x1)
          (-1 * nx + y1)
  where
    nx = x2 - x1
    ny = y2 - y1

    --Functions to scale lines, connect them and find their mid points.

scaleLine :: Float -> Line -> Line
scaleLine f (Line (Point x1 y1) (Point x2 y2))
  = Line  (Point x1 y1)
          (Point (x1 + (x2 -x1) * f)  (y1 + (y2 - y1) * f))

connectLine :: Line -> Line -> Line
connectLine l@(Line _ a@(Point lx2 ly2)) q@(Line (Point qx1 qy1) (Point qx2 qy2))
  = Line a b
    where
      diffx = 0-qx1 + lx2
      diffy = 0-qy1 + ly2
      b = Point (qx2 + diffx) (qy2 + diffy)

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) = Point ((x1+x2)/2) ((y1+y2)/2)

    -- Shape creation functions

rectangle :: Line -> Float -> Colour -> (PictureObject, Line)
rectangle ly@(Line a b) fact color = (Polygon [a,r1,r2,b] color Solid SolidFill, Line r1 r2)
  where
    r1 = rotate270 $ scaleLine fact (Line a b)
    r2 = rotate90  $ scaleLine fact (Line b a)

rightTri :: Line -> Float -> (PictureObject, Line, Line)
rightTri ly@(Line a b) angle = (Polygon [a,c,b] red Solid SolidFill, Line a c, Line c b)
  where
    mid = midpoint a b
    rotate@(Line _ c) = rotateLine (Line mid a) angle

triangles :: Line -> Float -> Float -> Colour -> (PictureObject, Line, Line)
triangles ly@(Line a b) angle scale color= (Polygon [a,c,b] color Solid SolidFill, Line a c, Line c b)
  where
    (Line _ po) = scaleLine scale ly
    rotate@(Line _ c) = rotateLine (Line po a) angle

polygon :: Int -> Line -> [Point]
polygon n ly@(Line a b)
  | n<0 = []
  |otherwise = [b]++(polygon (n-1) nex)
    where
      rotationAngle = (2 * pi) / (fromIntegral 3)
      nex = rotateLine ly rotationAngle
