{-# LANGUAGE NamedFieldPuns #-}

module Lib.Animation
  ( drawingAnimation
  , mkNoAnimationFn
  , mkNoAnimation
  , mkAnimation
  , update
  , animating
  , MkAnimation
  , Animation(..)
  ) where

-- TODO WIP

import Prelude hiding (repeat)
import Data.Maybe (isNothing)
import qualified Graphics.Gloss as G
import qualified ThirdParty.GraphicsGlossGame as G

type MkAnimation = Float -> G.Animation

data Animation = Animation
  { current :: G.Animation
  , make    :: Float -> G.Animation
  , loop    :: Bool
  }

mkNoAnimation :: Animation
mkNoAnimation = Animation G.noAnimation (const G.noAnimation) False

mkAnimation :: (Float -> G.Animation) -> Bool -> Animation
mkAnimation makeA loopA = Animation
  { current = makeA 0
  , make    = makeA
  , loop    = loopA
  }

update :: Float -> Animation -> Animation
update now animation
  | loop animation = repeat now animation
  | otherwise      = animation

restart :: Float -> Animation -> Animation
restart now animation = animation {current = make animation now}

isCurrentFinished :: Float -> Animation -> Bool
isCurrentFinished now Animation{current} = isNothing $ G.animationPicture current now

repeat :: Float -> Animation -> Animation
repeat now animation
  | isCurrentFinished now animation = restart now animation
  | otherwise                       = animation

animating :: Float -> Float -> Animation -> G.Scene world
animating w h Animation{current} = 
  G.translating (const (w, h)) $ G.animating (const current) G.blank

-- type AnimationFn = Float -> IO Picture

-- TODO maybe use data and constructors
-- Animation can only be created given the start time


mkNoAnimationFn :: MkAnimation
mkNoAnimationFn _ = G.noAnimation

drawingAnimation :: Int -> Int -> (world -> G.Animation) -> G.Scene world
drawingAnimation w h anim = G.translating (const (wf, hf)) $ G.animating anim G.blank
  where (wf, hf) = (fromIntegral w, fromIntegral h)