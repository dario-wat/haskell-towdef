module Debug 
  ( debugSpritesheet
  , debugSpritesheetFrames
  , debugSpritesheetFramesIndexed
  , debugTerrain
  , debugPoint
  , debugPointWithCoords
  , debugExampleTerrain
  , debugPath
  , gridArrayStr
  , gridArraysStr
  ) where

-- TODO WIP

import qualified Graphics.Gloss as G
import Lib.Image (readPngOrError, boundingBox)
import GameObjects.Sprite (draw, mkStaticSprite)
import qualified GameObjects.Sprite as S
import qualified GameObjects.Terrain as T
import Lib.Grid (Grid(..), emptyGrid, gridArrayStr, gridArraysStr)
import Lib.Spritesheet (Frame, allFrames, FrameIndex, framesIndexed)
import Lib.Path (genRandomPoints, connectTwoPoints, Path, connectAllPoints, createAllPaths, genStartEndPoints, isValidPath, gridifyPath, genRandomPath)
import Data.Array ((//))

debugSpriteBoundingBox :: S.Sprite -> G.Picture
debugSpriteBoundingBox (S.Sprite x y _ _ tile _) = G.translate x y $ boundingBox tile

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



debugExampleTerrain :: IO G.Picture
debugExampleTerrain = do
  tTil <- T.terrainTiles
  tObj <- T.terrainObjects
  let 
    --grassField = pictures [T.drawTile x y $ T.grass tTil | x <- [0..gridCols-1], y <- [0..gridRows-1]]
    terrainTiles = T.Terrain
      [ ( 0, 0, T.roadTopLeft tTil)
      , ( 2, 1, T.roadBottomRight tTil)
      , ( 3, 3, T.roadVertical tTil)
      , ( 3, 4, T.roadVertical tTil)
      , ( 3, 5, T.roadVertical tTil)
      , ( 3, 6, T.roadTopLeftSharp tTil)
      , ( 4, 6, T.roadHorizontal tTil)
      , ( 5, 6, T.roadHorizontal tTil)
      , ( 6, 6, T.roadHorizontal tTil)
      , ( 7, 6, T.roadCrossing tTil)
      , ( 7, 4, T.roadBottomLeft tTil)
      , ( 8, 6, T.roadHorizontal tTil)
      , ( 9, 6, T.roadHorizontal tTil)
      , (10, 6, T.horizontalBridge tObj)
      , ( 9, 4, T.roadTopRightSharp tTil)
      , ( 9, 3, T.roadBottomRightSharp tTil)
      , ( 8, 3, T.roadHorizontal tTil)
      ]
  return $ T.drawTerrain terrainTiles

debugGridPath :: Path -> Grid
debugGridPath path = Grid $ unGrid emptyGrid // zip path ['a'..]

debugPath :: IO ()
debugPath = do
  (startPoint, endPoint) <- genStartEndPoints
  middlePoints <- genRandomPoints 2
  let points = startPoint : middlePoints ++ [endPoint]
  putStrLn "\nPoints:"
  putStrLn $ gridArrayStr $ debugGridPath points
  -- putStrLn "\nConnect 2 points:"
  -- printGrids $ connectTwoPoints (head points) (points !! 1)
  -- putStrLn "\nConnect all points:"
  -- printGrids $ concat $ connectAllPoints points
  -- putStrLn "\nPaths:"
  -- printGrids $ createAllPaths points
  -- putStrLn "\nInvalid paths:"
  -- printGrids $ filter (not . isValidPath) $ createAllPaths points
  -- putStrLn "\nValid paths:"
  -- printGrids $ filter isValidPath $ createAllPaths points
  putStrLn "\nValid path:"
  path <- genRandomPath
  putStrLn $ gridArrayStr $ gridifyPath $ head $ filter isValidPath $ createAllPaths points
  where printGrids = putStrLn . gridArraysStr . map debugGridPath