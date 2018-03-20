module World where

import Algebra

-- | An object is just something in the world.
data Object = Object
  { objShape :: Shape
  , objColor :: Float2 -> Vec
  , objReflexivity :: Float
  , objTransparency :: Float
  , objReflIndex :: Float
  }

-- | Light is a given by location, color, direction, beam size (pi = full)
data Light = Light
  { lLocation :: Vec
  , lColor :: Vec
  , lDirection :: Vec
  , lBeam :: Float
  }

data Movement = MNone | MLeft | MRight | MUp | MDown | MEnter | MSpace | MRotateLeft | MRotateRight
  deriving Eq

-- | The world is the entire scene, containing objects and light sources,
--   as well as a camera with a field of view.
data World = World
  { wMovement        :: Movement
  , wReduceDepth     :: Bool
  , wObjects         :: [Object]
  , wLights          :: [Light]
  , wCamera          :: Ray
  , wFieldOfView     :: Float
  }

-- | A shape contains all the data for the linear algebra one might bring to
--   bear on an object 
data Shape
  = Sphere Vec Float -- Middle, radius
  | Plane Vec Float  -- Plane with equation P <> N + d = 0
  deriving Show

-- | Checkerboard and checkerboad-inspired textures
floorColor :: Float2 -> Vec
floorColor (x, y) = if (floor x + floor y) `mod` 2 == 0 then (0.8, 0.8, 0.8) else (0.2, 0.2, 0.2)

wallColor1 :: Float2 -> Vec
wallColor1 (x, y) = if floor (x / 6 + y / 6) `mod` 4 == 0 then (0.8, 0.8, 0.8) else (0.2, 0.2, 0.2)

wallColor2 :: Float2 -> Vec
wallColor2 (x, y) = if (floor (x / 6) - floor (y / 6)) `mod` 4 == 0 then (0.8, 0.8, 0.8) else (0.2, 0.2, 0.2)

sphereColor :: Float2 -> Vec
sphereColor (x, y) = if (floor (x * 3) + floor (y * 3)) `mod` 2 == 0 then (0.5, 0.2, 0.5) else (0.15, 0.1, 0.2)

-- | Our world for testing purposes
world :: World
world = World
  MNone
  False
  [ Object (Sphere (10,10,2) 2) (const (0.95,0.95,1)) 0.05 0.9 1.5
  , Object (Sphere (15,15,2) 1.5) sphereColor 0 0 0
  , Object (Sphere (10,15,2) 2.5) (const (0.55,0.35,0.2)) 0.1 0 0
  , Object (Sphere (8,5,2.5) 1.24) (const (1, 1, 1)) 1 0 0
  , Object (Plane (0,0,-1) 0.8) floorColor 0.3 0 0
  , Object (Plane (0,1,0) (-25)) wallColor1 0 0 0
  , Object (Plane (0,-1,0) (-25)) wallColor2 0 0 0
  ]
  [ Light (10, 10, 20) (200, 200, 200) (0,0,-1) (3 * pi / 8)
  , Light (10, 0, 10) (150,150,150) (-0.1,-0.1,-11) pi
  ]
  ((0, 0, 5), normalize (1, 1, 0))
  (pi / 2)
