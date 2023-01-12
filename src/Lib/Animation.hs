module Lib.Animation
  ( repeatingAnimation
  , drawingAnimation
  , mkNoAnimation
  , MkAnimation
  ) where

-- TOO WIP

import ThirdParty.GraphicsGlossGame (Animation, animationPicture, translating, animating, blank, Scene, noAnimation)
import Data.Maybe (isNothing)

-- TODO maybe use data and constructors
-- Animation can only be created given the start time
type MkAnimation = Float -> Animation

mkNoAnimation :: MkAnimation
mkNoAnimation _ = noAnimation

repeatingAnimation :: Animation -> MkAnimation -> Float -> Animation
repeatingAnimation currAnim newAnim t 
  | isAnimationFinished currAnim t = newAnim t  
  | otherwise                      = currAnim
  where isAnimationFinished anim = isNothing . animationPicture anim

drawingAnimation :: Int -> Int -> (world -> Animation) -> Scene world
drawingAnimation w h anim = translating (const (wf, hf)) $ animating anim blank
  where (wf, hf) = (fromIntegral w, fromIntegral h)