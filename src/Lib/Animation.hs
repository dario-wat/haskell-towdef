{-# LANGUAGE NamedFieldPuns #-}

module Lib.Animation
  ( Animation   -- Exporting only the type
  , MkAnimation
  , animating
  , mkNoAnimation
  , mkAnimation
  , update
  ) where

import Prelude hiding (repeat)
import Data.Maybe (isNothing)
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
update now animation@Animation{current, make, repeat}
  | not isCurrentFinished = animation
  | repeat == 0           = animation
  | repeat == -1          = animation {current = make now}
  | repeat > 0            = animation {current = make now, repeat = repeat - 1}
  | otherwise             = animation
  where isCurrentFinished = isNothing $ G.animationPicture current now

animating :: (world -> Animation) -> G.Scene world
animating worldAnim = G.animating (current . worldAnim) G.blank
