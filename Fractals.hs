import Codec.Picture
import GraphicsM
import GraphicsHelperFunctions
import PythogorusTree
import LsystemTree
import Mountains
import Grass
import KochSnowFlakes

-- The simplest looking file which combines all the fractals
-- note, the order of combination is extremely important as the opacity makes a difference.
-- You could think of them as layers in an art project/ 2d game.

picture = drawPicture 3 (mountain++mountainReflection++pTree2++pTree++ground++groundTree++groundTree2++flowers++snowFlakes)
file = writePng "final2.png" $ picture
