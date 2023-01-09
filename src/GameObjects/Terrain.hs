module GameObjects.Terrain
  ( cropTile
  , cropTiles
  , readTerrainImage
  , terrainObjects
  , terrainTiles
  , drawTile
  , drawTerrain
  , Tile(..)
  , Terrain
  , TerrainObjects(..)
  , TerrainTiles(..)
  ) where

import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Lib.Image (readPngOrError)
import Codec.Picture (DynamicImage, convertRGBA8)
import Codec.Picture.Extra (crop)
import Graphics.Gloss (Picture, pictures, translate)
import Const (spriteWidth, spriteHeight)
import Lib.Grid (gridRows, gridCols, gridCenterOf)
import Data.Tuple.HT (uncurry3)

data Tile = Tile 
  { picture :: Picture
  , width   :: Int
  , height  :: Int
  }

type Terrain = [(Int, Int, Tile)]

tileWidth :: Int
tileWidth = spriteWidth

tileHeight :: Int
tileHeight = spriteHeight

readTerrainImage :: IO DynamicImage
readTerrainImage = readPngOrError "assets/grass_tileset.png"

-- r and c are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Tile
cropTile r c = cropTiles r c 1 1

-- r and c are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Tile
cropTiles r c w h img = Tile (fromImageRGBA8 $ cropFn $ convertRGBA8 img) w h
  where cropFn = crop (c * tileWidth) (r * tileHeight) (w * tileWidth) (h * tileHeight)

data TerrainObjects = TerrainObjects
  { horizontalBridge    :: Tile
  , verticalBridge      :: Tile
  , greenTree1          :: Tile
  , greenTree2          :: Tile
  , greenTree3          :: Tile
  , greenTree4          :: Tile
  , brownTree1          :: Tile
  , brownTree2          :: Tile
  , brownTree3          :: Tile
  , brownTree4          :: Tile
  , rock1               :: Tile
  , rock2               :: Tile
  , rock3               :: Tile
  , rock4               :: Tile
  , bush1               :: Tile
  , bush2               :: Tile
  , rockWallDown        :: Tile
  , rockWallUp          :: Tile
  , rockWallLeft        :: Tile
  , rockWallRight       :: Tile
  , rockWallTopLeft     :: Tile
  , rockWallTopRight    :: Tile
  , rockWallBottomLeft  :: Tile
  , rockWallBottomRight :: Tile
  }

terrainObjects :: IO TerrainObjects
terrainObjects = do
  im <- readTerrainImage
  return $ TerrainObjects
    { horizontalBridge    = cropTiles 13  7 3 2 im
    , verticalBridge      = cropTiles 12 10 3 3 im
    , greenTree1          = cropTile   6 13 im
    , greenTree2          = cropTile   6 14 im
    , greenTree3          = cropTile   7 13 im
    , greenTree4          = cropTile   7 14 im
    , brownTree1          = cropTile   9 13 im
    , brownTree2          = cropTile   9 14 im
    , brownTree3          = cropTile  10 13 im
    , brownTree4          = cropTile  10 14 im
    , rock1               = cropTile  12 13 im
    , rock2               = cropTile  12 14 im
    , rock3               = cropTile  13 13 im
    , rock4               = cropTile  13 14 im
    , bush1               = cropTile   9 11 im
    , bush2               = cropTile   9 12 im
    , rockWallDown        = cropTiles  9  8  3 1 im
    , rockWallUp          = cropTiles 10  8  3 1 im
    , rockWallLeft        = cropTiles 10  6  1 3 im
    , rockWallRight       = cropTiles 10  7  1 3 im
    , rockWallTopLeft     = cropTile  11  8 im
    , rockWallTopRight    = cropTile  11  9 im
    , rockWallBottomLeft  = cropTile  12  8 im
    , rockWallBottomRight = cropTile  11  9 im
    }

data TerrainTiles = TerrainTiles
  { roadCrossing         :: Tile
  , roadTopLeft          :: Tile
  , roadTopRight         :: Tile
  , roadBottomLeft       :: Tile
  , roadBottomRight      :: Tile
  , roadTopLeftSharp     :: Tile
  , roadTopRightSharp    :: Tile
  , roadBottomLeftSharp  :: Tile
  , roadBottomRightSharp :: Tile
  , grass                :: Tile
  , roadVertical         :: Tile
  , roadHorizontal       :: Tile
  }

terrainTiles :: IO TerrainTiles
terrainTiles = do
  im <- readTerrainImage
  return $ TerrainTiles
    { roadCrossing         = cropTiles 6  1 3 3 im
    , roadTopLeft          = cropTiles 1 11 2 2 im
    , roadTopRight         = cropTiles 1 13 2 2 im
    , roadBottomLeft       = cropTiles 3 11 2 2 im
    , roadBottomRight      = cropTiles 3 13 2 2 im
    , roadTopLeftSharp     = cropTiles 1  5 2 2 im
    , roadTopRightSharp    = cropTiles 1  6 2 2 im
    , roadBottomLeftSharp  = cropTiles 2  5 2 2 im
    , roadBottomRightSharp = cropTiles 2  6 2 2 im
    , grass                = cropTile  1  2 im
    , roadVertical         = cropTile  2  5 im
    , roadHorizontal       = cropTile  1  6 im
    }

-- | Draws a tile onto the grid
-- Given row and column are the bottom left tile of multi cell tiles. E.g. if a
-- tile is 3x3, the given row and column will be the coordinates of the grid
-- where the bottom left cell will be drawn.
drawTile :: Int -> Int -> Tile -> Picture
drawTile x y (Tile picture w h) = translate xc yc picture
  where (xc, yc) = gridCenterOf (x, y) (w, h)

drawTerrain :: Terrain -> Picture
drawTerrain = pictures . map (uncurry3 drawTile)