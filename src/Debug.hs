{-# LANGUAGE NamedFieldPuns #-}

module Debug 
  ( debugSpritesheet
  , debugSpritesheetFrames
  -- , debugSpritesheetFramesIndexed
  , debugTerrain
  , debugPoint
  , debugPointWithCoords
  ) where

-- TODO WIP

import qualified Graphics.Gloss as G
import Lib.Image (readPngOrError, boundingBox)
import GameObjects.Sprite (draw, mkStaticSprite)
import qualified GameObjects.Sprite as S
import qualified GameObjects.Terrain as T
import Lib.Spritesheet (Frame (original, Frame, size, index), allFrames)
import GameObjects.Terrain (mkSpriteFromTile)

debugSpriteBoundingBox :: S.Sprite -> G.Picture
debugSpriteBoundingBox S.Sprite{S.x, S.y, S.vis=S.Pic tile} = 
  G.translate x y $ boundingBox tile
debugSpriteBoundingBox _ = error "debugSpriteBoundingBox: not implemented for animated sprites"

debugAndDrawSprite :: S.Sprite -> G.Picture
debugAndDrawSprite = G.pictures . sequence [debugSpriteBoundingBox, draw 0]

debugPoint :: Float -> Float -> G.Picture
debugPoint x y = G.translate x y $ G.color G.red $ G.circleSolid 5

coordinate :: Float -> Float -> G.Picture
coordinate x y = G.translate (x + xOff) y $ textScale $ G.text coordText
  where 
    xOff = 10
    coordText = show (round x :: Int, round y :: Int)
    textScale = G.scale 0.1 0.1

debugPointWithCoords :: Float -> Float -> G.Picture
debugPointWithCoords x y = G.pictures [debugPoint x y, coordinate x y]

debugSpritesheet :: Int -> Int -> FilePath -> IO G.Picture
debugSpritesheet w h imgPath = do
  img <- readPngOrError imgPath
  return $ debugSpritesheetFrames $ allFrames (w, h) img

-- debugSpritesheetFramesIndexed :: Int -> Int -> FilePath -> [FrameIndex] -> IO G.Picture
-- debugSpritesheetFramesIndexed w h imgPath coords = do
--   img <- readPngOrError imgPath
--   return $ debugSpritesheetFrames $ framesIndexed w h img coords

debugSpritesheetFrames :: [Frame] -> G.Picture
debugSpritesheetFrames fs =
  G.pictures $ map (debugAndDrawSprite . sprite) fs
  where 
    (xOff, yOff) = (-600, 300)
    padding = 10
    sprite Frame{original, index=(r, c), size=(w, h)} = mkStaticSprite 
      (fromIntegral $ c * (w + padding) + xOff) 
      (fromIntegral $ - r * (h + padding) + yOff)
      original

debugTerrain :: IO G.Picture
debugTerrain = do
  tObj <- T.terrainObjects
  tTil <- T.terrainTiles
  return $ G.pictures $ map debugAndDrawSprite
    [ mkSpriteFromTile ((-600),    300, T.horizontalBridge tObj)
    , mkSpriteFromTile ((-600),    100, T.verticalBridge tObj)
    , mkSpriteFromTile ((-650),  (-50), T.greenTree1 tObj)
    , mkSpriteFromTile ((-650), (-130), T.greenTree2 tObj)
    , mkSpriteFromTile ((-650), (-210), T.greenTree3 tObj)
    , mkSpriteFromTile ((-650), (-290), T.greenTree4 tObj)
    , mkSpriteFromTile ((-570),  (-50), T.brownTree1 tObj)
    , mkSpriteFromTile ((-570), (-130), T.brownTree2 tObj)
    , mkSpriteFromTile ((-570), (-210), T.brownTree3 tObj)
    , mkSpriteFromTile ((-570), (-290), T.brownTree4 tObj)
    , mkSpriteFromTile ((-490),  (-50), T.rock1 tObj)
    , mkSpriteFromTile ((-490), (-130), T.rock2 tObj)
    , mkSpriteFromTile ((-490), (-210), T.rock3 tObj)
    , mkSpriteFromTile ((-490), (-290), T.rock4 tObj)
    , mkSpriteFromTile ((-650), (-370), T.bush1 tObj)
    , mkSpriteFromTile ((-570), (-370), T.bush2 tObj)
    , mkSpriteFromTile ((-380),    350, T.rockWallDown tObj)
    , mkSpriteFromTile ((-380),    270, T.rockWallUp tObj)
    , mkSpriteFromTile ((-440),    100, T.rockWallLeft tObj)
    , mkSpriteFromTile ((-360),    100, T.rockWallRight tObj)
    , mkSpriteFromTile ((-410),  (-50), T.rockWallTopLeft tObj)
    , mkSpriteFromTile ((-410), (-130), T.rockWallTopRight tObj)
    , mkSpriteFromTile ((-410), (-210), T.rockWallBottomLeft tObj)
    , mkSpriteFromTile ((-410), (-290), T.rockWallBottomRight tObj)
    , mkSpriteFromTile ((-150),    300, T.roadCrossing tTil)
    , mkSpriteFromTile ((-230),    100, T.roadTopLeft tTil)
    , mkSpriteFromTile ((-230),  (-40), T.roadTopRight tTil)
    , mkSpriteFromTile ((-230), (-180), T.roadBottomLeft tTil)
    , mkSpriteFromTile ((-230), (-320), T.roadBottomRight tTil)
    , mkSpriteFromTile (    20,    300, T.roadVertical tTil)
    , mkSpriteFromTile (   100,    300, T.roadHorizontal tTil)
    , mkSpriteFromTile ( (-20),    100, T.roadTopLeftSharp tTil)
    , mkSpriteFromTile ( (-20),  (-40), T.roadTopRightSharp tTil)
    , mkSpriteFromTile ( (-20), (-180), T.roadBottomLeftSharp tTil)
    , mkSpriteFromTile ( (-20), (-320), T.roadBottomRightSharp tTil)
    , mkSpriteFromTile (   180,    300, T.grass tTil)
    ]
