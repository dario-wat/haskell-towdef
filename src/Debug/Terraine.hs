module Debug.Terraine
  ( debugTerraine
  ) where

import Graphics.Gloss (Picture, pictures, translate)
import GameObjects.Terraine
import GameObjects.Sprite hiding (Sprite(..))
import qualified GameObjects.Sprite as S (Sprite(..))
import Debug.Image (debugBoundingBox)

-- TODO only works for bitmaps
debugTileBoundingBox :: S.Sprite -> Picture
debugTileBoundingBox (S.Sprite x y tile) = translate x y $ debugBoundingBox tile

debugTerraine :: IO Picture
debugTerraine = do
  tObj <- terraineObjects
  tTil <- terraineTiles
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
    , mkSprite (-650) (-370) $ bush1 tObj
    , mkSprite (-570) (-370) $ bush2 tObj
    , mkSprite (-380)    350 $ rockWallDown tObj
    , mkSprite (-380)    270 $ rockWallUp tObj
    , mkSprite (-440)    100 $ rockWallLeft tObj
    , mkSprite (-360)    100 $ rockWallRight tObj
    , mkSprite (-410)  (-50) $ rockWallTopLeft tObj
    , mkSprite (-410) (-130) $ rockWallTopRight tObj
    , mkSprite (-410) (-210) $ rockWallBottomLeft tObj
    , mkSprite (-410) (-290) $ rockWallBottomRight tObj
    , mkSprite (-150)    300 $ roadCrossing tTil
    , mkSprite (-230)    100 $ roadTopLeft tTil
    , mkSprite (-230)  (-40) $ roadTopRight tTil
    , mkSprite (-230) (-180) $ roadBottomLeft tTil
    , mkSprite (-230) (-320) $ roadBottomRight tTil
    , mkSprite     20    300 $ roadStrip tTil
    , mkSprite  (-20)    100 $ roadTopLeftSharp tTil
    , mkSprite  (-20)  (-40) $ roadTopRightSharp tTil
    , mkSprite  (-20) (-180) $ roadBottomLeftSharp tTil
    , mkSprite  (-20) (-320) $ roadBottomRightSharp tTil
    , mkSprite    150    300 $ grass tTil
    ]
  where debugAndDrawTile = pictures . sequence [debugTileBoundingBox, draw]