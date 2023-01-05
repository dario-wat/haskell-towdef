module Debug.Terraine
  ( debugTerraine
  ) where

import Graphics.Gloss (Picture, pictures, translate)
import GameObjects.Terraine
import GameObjects.Sprite
import Debug.Image (debugBoundingBox)

-- TODO only works for bitmaps
debugTileBoundingBox :: Sprite -> Picture
debugTileBoundingBox (Sprite x y tile) = translate x y $ debugBoundingBox tile

debugTerraine :: IO Picture
debugTerraine = do
  tObj <- terraineObjects
  return $ pictures $ map debugAndDrawTile
    [ mkSprite (-600)    300 $ horizontalBridge tObj
    , mkSprite (-600)    100 $ verticalBridge tObj
    , mkSprite (-650)  (-50) $ greenTree1 tObj
    , mkSprite (-650) (-130) $ greenTree2 tObj
    , mkSprite (-650) (-210) $ greenTree3 tObj
    , mkSprite (-650) (-290) $ greenTree4 tObj
    , mkSprite (-570)  (-50) $ brownTree1 tObj
    , mkSprite (-570) (-130) $ brownTree2 tObj
    , mkSprite (-570) (-210) $ brownTree3 tObj
    , mkSprite (-570) (-290) $ brownTree4 tObj
    , mkSprite (-490)  (-50) $ rock1 tObj
    , mkSprite (-490) (-130) $ rock2 tObj
    , mkSprite (-490) (-210) $ rock3 tObj
    , mkSprite (-490) (-290) $ rock4 tObj
    ]
  where debugAndDrawTile = pictures . sequence [debugTileBoundingBox, draw]