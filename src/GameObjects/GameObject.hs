module GameObjects.GameObject 
  ( GameObject(..)
  ) where

import qualified Graphics.Gloss as G

class GameObject a where
  update :: Float -> a -> a
  draw   :: Float -> a -> G.Picture