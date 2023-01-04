module Lib.Image 
  ( 
  ) where

import Codec.Picture (readPng, DynamicImage)
import Codec.Picture.Extra (crop)
import Graphics.Gloss (Picture)

-- data Terraine = Terraine 
--   {

--   }

-- terrainImage :: IO Picture
-- terrainImage = crop 128 128 64 64 <$> image
--   where image = readPng "assets/Grass Tileset.png"

