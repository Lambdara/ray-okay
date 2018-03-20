import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.DeepSeq
import Control.Monad
import System.IO.Unsafe
import Foreign.ForeignPtr
import qualified Foreign.Ptr as P
import qualified Data.ByteString as B
import qualified Foreign.Storable as S
import Data.Word
import Tracer
import Algebra
import World

width = 512
height = 512
maxDepth = 7

event :: Event -> World -> World
event (EventKey (SpecialKey KeyLeft) Down _ _) w = w{ wMovement=MLeft }
event (EventKey (SpecialKey KeyRight) Down _ _) w = w{ wMovement=MRight }
event (EventKey (SpecialKey KeyUp) Down _ _) w = w{ wMovement=MUp }
event (EventKey (SpecialKey KeyDown) Down _ _) w = w{ wMovement=MDown }
event (EventKey (SpecialKey KeyEnter) Down _ _) w = w{ wMovement=MEnter }
event (EventKey (SpecialKey KeySpace) Down _ _) w = w{ wMovement=MSpace }
event (EventKey (Char ',') Down _ _) w = w{ wMovement=MRotateLeft }
event (EventKey (Char '.') Down _ _) w = w{ wMovement=MRotateRight }
event (EventKey (Char 't') Down _ _) w = w{ wReduceDepth = not $ wReduceDepth w }
event (EventKey _ Up _ _) w = w{ wMovement=MNone }
event _ w = w

step :: Float -> World -> World
step dt w@World{ wMovement = mov, wCamera=(cOrigin, cDir) } = w{ wCamera=(cOrigin', cDir') }
  where
    up = (0, 0, 1)
    right = cDir >< up
    cOrigin' = cOrigin + right @* (dt * movementRight mov) + cDir @* (dt * movementAhead mov) + up @* (dt * movementUp mov)
    cDir' = normalize (cDir + right @* (dt * movementRotate mov))

movementRight :: Movement -> Float
movementRight MRight = 1
movementRight MLeft  = -1
movementRight _      = 0

movementAhead :: Movement -> Float
movementAhead MUp   = 1
movementAhead MDown = -1
movementAhead _     = 0

movementUp :: Movement -> Float
movementUp MEnter = 1
movementUp MSpace = -1
movementUp _      = 0

movementRotate :: Movement -> Float
movementRotate MRotateLeft  = -1
movementRotate MRotateRight = 1
movementRotate _            = 0

-- | Main function of the application
main = play (InWindow "Ray Okay" (width, height) (10, 10)) black 60 world draw event step

-- | Draw maps a resolution with a world to a picture of this world
draw :: World -> Picture
draw w@World{ wFieldOfView=vof, wCamera=(cOrigin,cDir), wMovement=mov, wReduceDepth=reduce } = Scale factorScale factorScale $ bitmapOfForeignPtr scaledWidth scaledHeight (BitmapFormat BottomToTop PxABGR) (unsafePerformIO createBytes) False
  where
    createBytes = do
      ptr <- mallocForeignPtrArray (4 * scaledWidth * scaledHeight)
      withForeignPtr ptr setBytes
      return ptr
    setBytes ptr = forM_ [(i,j) | i <- [0 .. scaledWidth - 1], j <- [0 .. scaledHeight - 1]] (setPixel ptr)
    depth = if reduce then 3 else maxDepth
    (up, right) = plane vof cDir
    leftBottom  = cDir - (up @/ 2) - (right @/ 2) -- Left bottom of screen relative to cOrigin
    setPixel ptr (i, j) = do
      S.pokeElemOff ptr id 255
      S.pokeElemOff ptr (id + 1) $ toWord8 b
      S.pokeElemOff ptr (id + 2) $ toWord8 g
      S.pokeElemOff ptr (id + 3) $ toWord8 r
      where
        id = (j * scaledWidth + i) * 4
        (r, g, b) = cast w depth $!! (cOrigin, normalize $ leftBottom + right @* (intToFloat i / intToFloat scaledWidth) + up @* (intToFloat j / intToFloat scaledHeight))
    factor = if reduce then 1/2 else 2
    factorScale = 1 / factor
    scaledWidth = round $ fromIntegral width * factor
    scaledHeight = round $ fromIntegral height * factor

-- | Converts `Float` to single byte (`Word8`)
toWord8 :: Float -> Word8
toWord8 = round . (255 *) . min 1.0
