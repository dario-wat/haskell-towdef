module Debug 
  ( debugSpritesheet
  , debugSpritesheetFrames
  , debugSpritesheetFramesIndexed
  , debugTerraine
  , debugPoint
  , debugPointWithCoords
  , debugGrid
  ) where

import qualified Graphics.Gloss as G
import Lib.Image (readPngOrError)
import GameObjects.Sprite (draw, mkStaticSprite)
import qualified GameObjects.Sprite as S
import qualified GameObjects.Terraine as T
import Lib.Window (windowBottomLeft, windowTopRight)
import Lib.Grid (cellSize)
import Graphics.Gloss (Picture, pictures, rectangleWire)
import Lib.Spritesheet (Frame, allFrames, FrameIndex, framesIndexed)

-- Draw a rectangle with a border of given thickness. Complexity is O(n) where
-- n is the thickness so we shouldn't use this with high numbers
thickRectangleWire :: Float -> Float -> Float -> Picture
thickRectangleWire thickness w h = 
  pictures $ map rectangleWithThickness $ filter isValidThickness [minTh..maxTh]
  where 
    floorF = fromIntegral . (floor :: Float -> Int)
    (minTh, maxTh) = (floorF $ -thickness / 2, floorF thickness / 2)
    isValidThickness t = w + t >= 0 && h + t >= 0
    rectangleWithThickness t = rectangleWire (w + t) (h + t)

bitmapSizeF :: G.BitmapData -> (Float, Float)
bitmapSizeF bData = (fromIntegral w, fromIntegral h)
  where (w, h) = G.bitmapSize bData

-- NOTE works only on bitmaps
debugBoundingBox :: G.Picture -> G.Picture
debugBoundingBox (G.Bitmap bData) = G.color G.red $ thickRectangleWire 2 w h
  where (w, h) = bitmapSizeF bData
debugBoundingBox _              = error "boundingBox: not a bitmap"

debugSpriteBoundingBox :: S.Sprite -> G.Picture
debugSpriteBoundingBox (S.Sprite x y _ _ tile _) = G.translate x y $ debugBoundingBox tile

debugAndDrawSprite :: S.Sprite -> G.Picture
debugAndDrawSprite = G.pictures . sequence [debugSpriteBoundingBox, draw]

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
  return $ debugSpritesheetFrames $ allFrames w h img

debugSpritesheetFramesIndexed :: Int -> Int -> FilePath -> [FrameIndex] -> IO G.Picture
debugSpritesheetFramesIndexed w h imgPath coords = do
  img <- readPngOrError imgPath
  return $ debugSpritesheetFrames $ framesIndexed w h img coords

debugSpritesheetFrames :: [Frame] -> G.Picture
debugSpritesheetFrames fs =
  G.pictures $ map (\(i, pic, s) -> debugAndDrawSprite $ sprite i s pic) fs
  where 
    (xOff, yOff) = (-600, 300)
    padding = 10
    sprite (r, c) (w, h) = mkStaticSprite 
      (fromIntegral $ c * (w + padding) + xOff) 
      (fromIntegral $ - r * (h + padding) + yOff)

debugTerraine :: IO G.Picture
debugTerraine = do
  tObj <- T.terraineObjects
  tTil <- T.terraineTiles
  return $ G.pictures $ map debugAndDrawSprite
    [ mkStaticSprite (-600)    300 $ T.horizontalBridge tObj
    , mkStaticSprite (-600)    100 $ T.verticalBridge tObj
    , mkStaticSprite (-650)  (-50) $ T.greenTree1 tObj
    , mkStaticSprite (-650) (-130) $ T.greenTree2 tObj
    , mkStaticSprite (-650) (-210) $ T.greenTree3 tObj
    , mkStaticSprite (-650) (-290) $ T.greenTree4 tObj
    , mkStaticSprite (-570)  (-50) $ T.brownTree1 tObj
    , mkStaticSprite (-570) (-130) $ T.brownTree2 tObj
    , mkStaticSprite (-570) (-210) $ T.brownTree3 tObj
    , mkStaticSprite (-570) (-290) $ T.brownTree4 tObj
    , mkStaticSprite (-490)  (-50) $ T.rock1 tObj
    , mkStaticSprite (-490) (-130) $ T.rock2 tObj
    , mkStaticSprite (-490) (-210) $ T.rock3 tObj
    , mkStaticSprite (-490) (-290) $ T.rock4 tObj
    , mkStaticSprite (-650) (-370) $ T.bush1 tObj
    , mkStaticSprite (-570) (-370) $ T.bush2 tObj
    , mkStaticSprite (-380)    350 $ T.rockWallDown tObj
    , mkStaticSprite (-380)    270 $ T.rockWallUp tObj
    , mkStaticSprite (-440)    100 $ T.rockWallLeft tObj
    , mkStaticSprite (-360)    100 $ T.rockWallRight tObj
    , mkStaticSprite (-410)  (-50) $ T.rockWallTopLeft tObj
    , mkStaticSprite (-410) (-130) $ T.rockWallTopRight tObj
    , mkStaticSprite (-410) (-210) $ T.rockWallBottomLeft tObj
    , mkStaticSprite (-410) (-290) $ T.rockWallBottomRight tObj
    , mkStaticSprite (-150)    300 $ T.roadCrossing tTil
    , mkStaticSprite (-230)    100 $ T.roadTopLeft tTil
    , mkStaticSprite (-230)  (-40) $ T.roadTopRight tTil
    , mkStaticSprite (-230) (-180) $ T.roadBottomLeft tTil
    , mkStaticSprite (-230) (-320) $ T.roadBottomRight tTil
    , mkStaticSprite     20    300 $ T.roadStrip tTil
    , mkStaticSprite  (-20)    100 $ T.roadTopLeftSharp tTil
    , mkStaticSprite  (-20)  (-40) $ T.roadTopRightSharp tTil
    , mkStaticSprite  (-20) (-180) $ T.roadBottomLeftSharp tTil
    , mkStaticSprite  (-20) (-320) $ T.roadBottomRightSharp tTil
    , mkStaticSprite    150    300 $ T.grass tTil
    ]

-- TODO right now it draws the entire window, but it should be bounded
-- by a rectangle. It should use grid size and cell size to determine
debugGrid :: G.Picture
debugGrid = G.pictures $ map G.line $ verticalLines ++ horizontalLines
  where
    (startX, startY) = windowBottomLeft
    (endX, endY) = windowTopRight
    cellSizeF = fromIntegral cellSize
    verticalLines   = map (\x -> [(x, startY), (x, endY)]) [startX, startX + cellSizeF .. endX]
    horizontalLines = map (\y -> [(startX, y), (endX, y)]) [startY, startY + cellSizeF .. endY]