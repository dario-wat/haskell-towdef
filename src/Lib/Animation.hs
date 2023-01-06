module Lib.Animation
  ( repeatingAnimation
  , AnimationBuilder
  ) where

import ThirdParty.GraphicsGlossGame (Animation, animationPicture)
import Data.Maybe (isNothing)

-- Animation can only be created given the start time
type AnimationBuilder = Float -> Animation

repeatingAnimation :: Animation -> AnimationBuilder -> Float -> Animation
repeatingAnimation currAnim newAnim t 
  | isAnimationFinished currAnim t = newAnim t  
  | otherwise                      = currAnim
  where isAnimationFinished anim = isNothing . animationPicture anim
