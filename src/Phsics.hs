module Phsics
    ( someFunc
    ) where

import Linear.V3
import Linear.Vector

import Data.List
import Data.Maybe

someFunc :: IO ()
someFunc = do
  let ps = PhsicsSimulation [Body (V3 0 0 0) (V3 0 0 0 ) Point] SimulationSettings
  print (tick 0.5 $ tick 0.5 ps)
  
data PhsicsSimulation = PhsicsSimulation{
  objects :: [Body], 
  settings :: SimulationSettings
} deriving (Show, Read)

data SimulationSettings = SimulationSettings deriving (Show, Read)

data Body = Body {
  position :: V3 Double,
  velocity :: V3 Double,
  shape :: CollisionShape
} deriving (Show, Read, Eq)

data CollisionShape = Point | Sphere Double deriving (Show, Read, Eq)

tick :: Double -> PhsicsSimulation -> PhsicsSimulation
tick dT ps = detectAndHandleCollisions $ integrateVelocities dT $ withObjects (applyGravity $ V3 0 (9.81 * dT) 0) ps

integrateVelocities :: Double -> PhsicsSimulation -> PhsicsSimulation
integrateVelocities dT ps = withObjects (\body -> body{position = (position body) ^+^ (dT *^ velocity body)}) ps

withObjects :: (Body -> Body) -> PhsicsSimulation -> PhsicsSimulation
withObjects op (PhsicsSimulation objs settings) = PhsicsSimulation (map op objs) settings

applyGravity :: V3 Double -> Body -> Body
applyGravity grav (Body p v shape) = Body p (v ^+^ grav) shape

radius :: CollisionShape -> Double
radius Point = 0

--TODO: This is a really crude implementation. It is likely good enough to get feet wet.
--  Has rather bad performance though; This implementation could serve as a test specification though.
detectAndHandleCollisions :: PhsicsSimulation -> PhsicsSimulation
detectAndHandleCollisions ps@(PhsicsSimulation bodies settings) = ps{objects = transformed}
  where transformed = map (\body -> applyCollisionResults body $ map fst $ catMaybes $ map (\otherB -> collides body otherB) (delete body bodies)) bodies

data CollisionResult = CollisionResult{
  contactPoint :: V3 Double,
  contactNormal :: V3 Double,
  otherBody :: Body
}

--TODO: Spec: applyCollisionResults a lst = ~ shuffle lst
applyCollisionResults :: Body -> [CollisionResult] -> Body
applyCollisionResults = undefined

len2 :: V3 Double -> Double
len2 (V3 a b c) = a*a+b*b+c*c

--TODO: Spec: Left result belonds to left body
collides :: Body -> Body -> Maybe (CollisionResult, CollisionResult)
collides a b
  | dPos2 > (radius (shape a) + radius (shape b)) = Nothing
  | otherwise = Just (CollisionResult zero zero a, CollisionResult zero zero b)
  where dPos2 = len2 (position a ^-^ position b)
