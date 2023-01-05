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
    [ mkTile (-600) 300 $ horizontalBridge tObj
    , mkTile (-600) 100 $ verticalBridge tObj
    ]
  where debugAndDrawTile = pictures . sequence [debugTileBoundingBox, drawTile]