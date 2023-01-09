module Debug 
  ( debugSpritesheet
  , debugSpritesheetFrames
  , debugSpritesheetFramesIndexed
  , debugTerrain
  , debugPoint
  , debugPointWithCoords
  , debugGrid
  , debugExampleTerrain
  ) where

import qualified Graphics.Gloss as G
import Lib.Image (readPngOrError)
import GameObjects.Sprite (draw, mkStaticSprite)
import qualified GameObjects.Sprite as S
import qualified GameObjects.Terrain as T
import Lib.Grid (gridWidth, gridHeight, gridX, gridY, gridX, gridY, gridRows, gridCols, gridStartY, gridStartX)
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

debugTerrain :: IO G.Picture
debugTerrain = do
  tObj <- T.terrainObjects
  tTil <- T.terrainTiles
  return $ G.pictures $ map debugAndDrawSprite
    [ mkStaticSprite (-600)    300 $ T.picture $ T.horizontalBridge tObj
    , mkStaticSprite (-600)    100 $ T.picture $ T.verticalBridge tObj
    , mkStaticSprite (-650)  (-50) $ T.picture $ T.greenTree1 tObj
    , mkStaticSprite (-650) (-130) $ T.picture $ T.greenTree2 tObj
    , mkStaticSprite (-650) (-210) $ T.picture $ T.greenTree3 tObj
    , mkStaticSprite (-650) (-290) $ T.picture $ T.greenTree4 tObj
    , mkStaticSprite (-570)  (-50) $ T.picture $ T.brownTree1 tObj
    , mkStaticSprite (-570) (-130) $ T.picture $ T.brownTree2 tObj
    , mkStaticSprite (-570) (-210) $ T.picture $ T.brownTree3 tObj
    , mkStaticSprite (-570) (-290) $ T.picture $ T.brownTree4 tObj
    , mkStaticSprite (-490)  (-50) $ T.picture $ T.rock1 tObj
    , mkStaticSprite (-490) (-130) $ T.picture $ T.rock2 tObj
    , mkStaticSprite (-490) (-210) $ T.picture $ T.rock3 tObj
    , mkStaticSprite (-490) (-290) $ T.picture $ T.rock4 tObj
    , mkStaticSprite (-650) (-370) $ T.picture $ T.bush1 tObj
    , mkStaticSprite (-570) (-370) $ T.picture $ T.bush2 tObj
    , mkStaticSprite (-380)    350 $ T.picture $ T.rockWallDown tObj
    , mkStaticSprite (-380)    270 $ T.picture $ T.rockWallUp tObj
    , mkStaticSprite (-440)    100 $ T.picture $ T.rockWallLeft tObj
    , mkStaticSprite (-360)    100 $ T.picture $ T.rockWallRight tObj
    , mkStaticSprite (-410)  (-50) $ T.picture $ T.rockWallTopLeft tObj
    , mkStaticSprite (-410) (-130) $ T.picture $ T.rockWallTopRight tObj
    , mkStaticSprite (-410) (-210) $ T.picture $ T.rockWallBottomLeft tObj
    , mkStaticSprite (-410) (-290) $ T.picture $ T.rockWallBottomRight tObj
    , mkStaticSprite (-150)    300 $ T.picture $ T.roadCrossing tTil
    , mkStaticSprite (-230)    100 $ T.picture $ T.roadTopLeft tTil
    , mkStaticSprite (-230)  (-40) $ T.picture $ T.roadTopRight tTil
    , mkStaticSprite (-230) (-180) $ T.picture $ T.roadBottomLeft tTil
    , mkStaticSprite (-230) (-320) $ T.picture $ T.roadBottomRight tTil
    , mkStaticSprite     20    300 $ T.picture $ T.roadVertical tTil
    , mkStaticSprite    100    300 $ T.picture $ T.roadHorizontal tTil
    , mkStaticSprite  (-20)    100 $ T.picture $ T.roadTopLeftSharp tTil
    , mkStaticSprite  (-20)  (-40) $ T.picture $ T.roadTopRightSharp tTil
    , mkStaticSprite  (-20) (-180) $ T.picture $ T.roadBottomLeftSharp tTil
    , mkStaticSprite  (-20) (-320) $ T.picture $ T.roadBottomRightSharp tTil
    , mkStaticSprite    180    300 $ T.picture $ T.grass tTil
    ]

debugGrid :: G.Picture
debugGrid = G.pictures [vLines, hLines, vNumbers, hNumbers]
  where
    (endX, endY) = (gridStartX + gridWidth, gridStartY + gridHeight)
    gLines = G.pictures . map G.line
    vLines = gLines [[(gridX x, gridStartY), (gridX x, endY)] | x <- [0..gridCols]]
    hLines = gLines [[(gridStartX, gridY y), (endX, gridY y)] | y <- [0..gridRows]]
    text = G.scale 0.1 0.1 . G.text . show
    vNumbers = G.pictures [G.translate (gridX x + 20) (endY + 16) $ text x | x <- [0..gridCols-1]]
    hNumbers = G.pictures [G.translate (gridStartX - 20) (gridY y + 20) $ text y | y <- [0..gridRows-1]]

debugExampleTerrain :: IO G.Picture
debugExampleTerrain = do
  tTil <- T.terrainTiles
  tObj <- T.terrainObjects
  let 
    grassField = pictures [T.drawTile x y $ T.grass tTil | x <- [0..gridCols-1], y <- [0..gridRows-1]]
    terrainTiles = 
      [ (0, 0, T.roadTopLeft tTil)
      , (2, 1, T.roadBottomRight tTil)
      , (3, 3, T.roadVertical tTil)
      , (3, 4, T.roadVertical tTil)
      , (3, 5, T.roadTopLeftSharp tTil)
      ]
  return $ T.drawTerrain terrainTiles