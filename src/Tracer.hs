module Tracer where

import Control.DeepSeq
import Data.Maybe
import Algebra
import World

-- | Convert Vec to one which is positive in the z-axis
vUp :: Vec -> Vec
vUp v@(_,_,z) = if z < 0 then -v else v

-- | Make a plane such that P <> N + d = 0
plane :: Float -> Vec -> (Vec, Vec)
plane fieldOfView dir@(x,y,z) = (up @* length, dir >< up @* length)
  where
    up = normalize $ vUp $ (-y,x,0) >< dir
    length = tan (fieldOfView / 2) * 2

-- | Infinity
inf :: Float
inf = 1/0

-- | Offset amount for creating secondary rays
delta :: Float
delta = 0.001

-- | Offset a ray
offset :: Ray -> Ray
offset (origin, direction) = (origin + direction @* delta, direction)

-- | Given a world, a maximum recursion depth, and a ray, computes a color
--   'seen' by the ray
cast :: World -> Int -> Ray -> Vec
cast _ 0 _ = v0
cast w depth ray@(o,l) = maybe v0 intersection (intersect inf (wObjects w) ray)
  where
    cast' = (cast w (depth - 1) $!!)
    intersection (obj, d)
      = ((color * lights w sn loc) @* (1 - objReflexivity obj - objTransparency obj)) -- The object
      + ((color * cast' (offset (loc, reflect sn l))) @* reflexivity) -- Reflections
      + ((color * cast' (offset (loc, refracted (objShape obj)))) @* transparency) -- Transparency
      where
        shape = objShape obj 
        color = objColor obj $ textureLocation shape loc
        sn = aim (-l) $ normal shape loc -- Surface normal
        loc = o + (l @* d) -- Location of the intersection
        shapes = map objShape $ wObjects w -- All shapes in the world
        fr = schlick sn n1 n2 l .* objTransparency obj -- Factor of reflexion based on schlicks law
        reflexivity = objReflexivity obj + fr
        transparency = objTransparency obj - fr
        refracted shp@(Sphere _ _) = refract sn n1 n2 l
        refracted shp@(Plane _ _) = l
        (n1,n2) = if (sn <> l) < 0 -- Sort the refractive indices
                  then (1,objReflIndex obj)
                  else (objReflIndex obj,1)

-- | Takes the world, a location, a service normal, and a shadow ray and 
--   computes ligthing of the object using a shadow ray
lights :: World -> Vec -> Vec -> Vec
lights w sn p = sum $ map intensity (wLights w)
  where
    intensity (Light pos i dr beam) =
      if (dr <> (-dir)) / (vLength dr + vLength dir) > cos beam -- If the shadow ray is within the beam
      then maybe (i @* (max 0 (sn <> dir) / dist ^ 2)) (const v0) (intersect (dist - delta) (wObjects w) $ offset (p, dir))
      else v0
      where
        dist = vLength (p - pos)
        dir = (pos - p) @/ dist

-- | Reports which object is the first hit and at which distance
intersect :: Float -> [Object] -> Ray -> Maybe (Object, Float)
intersect tMax (x:xs) ray = maybe (next tMax) match (intersectShape ray $ objShape x)
  where
    match t = if t < tMax then Just $ fromMaybe (x, t) (next t) else next tMax
    next t = intersect t xs ray
intersect _ _ _ = Nothing

-- | Reports at which distance (if any) the ray hits the shape
intersectShape :: Ray -> Shape -> Maybe Float
intersectShape (origin, direction) (Sphere center radius) =
  if d < 0
  then Nothing
  else listToMaybe $ filter (>=0) [ans (+), ans (-)]
  where
    oc = origin - center
    b = direction <> oc
    d = b ^ 2 - (vLengthSquared oc - radius ^ 2)
    ans f = - b `f` sqrt d
intersectShape (origin, direction) (Plane normal d) =
  if t > 0
  then Just t
  else Nothing
  where
    t = (d - origin <> normal) / (direction <> normal)

-- | Create surface normal of shape at point
normal :: Shape -> Vec -> Vec
normal (Sphere center radius) p = normalize $ (p - center) @/ radius
normal (Plane normal _) _ = normal

-- | Tranform point on shape to point on texture
textureLocation :: Shape -> Vec -> (Float, Float)
textureLocation (Sphere center radius) pos = (acos (z / radius), atan (y / x))
  where
    (x, y, z) = pos - center
textureLocation (Plane normal@(a,b,c) _) pos = (u <> pos, v <> pos)
  where
    u = normalize $ if a == 0 then (0, -c, b) else (b, -a, 0)
    v = normal >< u

-- | Reflection on surface with provided normal
reflect :: Vec -> Vec -> Vec
reflect nor dir = dir - nor @* 2 @* (dir <> nor)

-- | Refraction on surface with provided normal and refraction indices
refract :: Vec -> Float -> Float -> Vec -> Vec
refract n n1 n2 v =
  (v @* r) - (n @* (r * c + sqrt (1 - r ^ 2 * (1 - c ^ 2))))
  where
    r = n1 / n2
    c = n <> v

-- | Implementation of schlick
schlick :: Vec -> Float -> Float -> Vec -> Float
schlick n n1 n2 v = r0 + (1 - r0) * (1 - cos (n <> v / (vLength n * vLength v))) ^ 5
  where
    r0 = ((n1 - n2) / (n1 + n2)) ^ 2
