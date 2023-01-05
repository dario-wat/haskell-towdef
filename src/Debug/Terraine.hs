module Debug.Terraine
  ( debugTerraine
  ) where

import Graphics.Gloss (Picture, pictures, translate)
import Lib.Image.Terraine
import Debug.Image (debugBoundingBox)

debugTileBoundingBox :: Tile -> Picture
debugTileBoundingBox (Tile x y tile) = translate x y $ debugBoundingBox tile

debugTerraine :: IO Picture
debugTerraine = do
  tObj <- terraineObjects
  return $ pictures $ map debugAndDrawTile
    [ mkTile (-600)    300 $ horizontalBridge tObj
    , mkTile (-600)    100 $ verticalBridge tObj
    , mkTile (-650)  (-50) $ greenTree1 tObj
    , mkTile (-650) (-130) $ greenTree2 tObj
    , mkTile (-650) (-210) $ greenTree3 tObj
    , mkTile (-650) (-290) $ greenTree4 tObj
    , mkTile (-570)  (-50) $ brownTree1 tObj
    , mkTile (-570) (-130) $ brownTree2 tObj
    , mkTile (-570) (-210) $ brownTree3 tObj
    , mkTile (-570) (-290) $ brownTree4 tObj
    , mkTile (-490)  (-50) $ rock1 tObj
    , mkTile (-490) (-130) $ rock2 tObj
    , mkTile (-490) (-210) $ rock3 tObj
    , mkTile (-490) (-290) $ rock4 tObj
    ]
  where debugAndDrawTile = pictures . sequence [debugTileBoundingBox, drawTile]