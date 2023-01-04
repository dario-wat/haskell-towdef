module Lib.Image
  ( readPngOrError
  ) where

import Codec.Picture (DynamicImage, readPng)
import Data.Either (fromRight)

readPngOrError :: FilePath -> IO DynamicImage
readPngOrError filepath = fromRight (error errorText) <$> readPng filepath
  where errorText = "no image: " <> filepath