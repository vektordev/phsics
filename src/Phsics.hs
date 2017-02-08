module Phsics
    ( someFunc
    ) where

import Linear.V3

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data PhsicsSimulation = PhysicsSimulation{
  objects :: [Body]
}

data Body = Body {
  position :: V3 Double,
  velocity :: V3 Double,
  shape :: CollisionShape
}

data CollisionShape = Point
