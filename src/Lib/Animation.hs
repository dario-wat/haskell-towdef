{-# LANGUAGE NamedFieldPuns #-}

module Lib.Animation
  ( Animation   -- Exporting only the type
  , MkAnimation
  , animating
  , draw
  , mkNoAnimation
  , mkAnimation
  , update
  ) where

import Prelude hiding (repeat)
import Data.Maybe (isNothing, fromMaybe)
import qualified Graphics.Gloss as G
import qualified ThirdParty.GraphicsGlossGame as G

type MkAnimation = Float -> G.Animation

data Animation = Animation
  { current :: !G.Animation
  , make    :: !MkAnimation
  , repeat  :: !Int   -- Number of times to repeat the animation (-1 for infinite)
  }

mkNoAnimation :: Animation
mkNoAnimation = Animation 
  { current = G.noAnimation
  , make    = const G.noAnimation
  , repeat  = 0
  }

mkAnimation :: (Float -> G.Animation) -> Int -> Animation
mkAnimation makeA repeatA = Animation
  { current = makeA 0
  , make    = makeA
  , repeat  = repeatA
  }

update :: Float -> Animation -> Animation
update time animation@Animation{current, make, repeat}
  | not isCurrentFinished = animation
  | repeat == 0           = animation
  | repeat == -1          = animation {current = make time}
  | repeat > 0            = animation {current = make time, repeat = repeat - 1}
  | otherwise             = animation
  where isCurrentFinished = isNothing $ G.animationPicture current time

draw :: Animation -> Float -> G.Picture
draw Animation{current} = fromMaybe G.blank . G.animationPicture current

animating :: (world -> Animation) -> G.Scene world
animating worldToAnim = G.animating (current . worldToAnim) G.blank
