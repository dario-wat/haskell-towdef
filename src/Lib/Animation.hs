{-# LANGUAGE NamedFieldPuns #-}

module Lib.Animation
  ( mkNoAnimationFn
  , mkNoAnimation
  , mkAnimation
  , update
  , animating
  , MkAnimation
  , Animation   -- Exporting only the type
  ) where

-- TODO WIP

import Prelude hiding (repeat)
import Data.Maybe (isNothing)
import qualified Graphics.Gloss as G
import qualified ThirdParty.GraphicsGlossGame as G

type MkAnimation = Float -> G.Animation

data Animation = Animation
  { current :: G.Animation
  , make    :: MkAnimation
  , repeat  :: Int   -- Number of times to repeat the animation (-1 for infinite)
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

animating :: Float -> Float -> (world -> Animation) -> G.Scene world
animating w h worldAnim = 
  G.translating (const (w, h)) $ G.animating (current . worldAnim) G.blank

-- type AnimationFn = Float -> IO Picture

-- TODO maybe use data and constructors
-- Animation can only be created given the start time


mkNoAnimationFn :: MkAnimation
mkNoAnimationFn _ = G.noAnimation
