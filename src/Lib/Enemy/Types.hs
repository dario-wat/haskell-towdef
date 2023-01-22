{-# LANGUAGE DeriveGeneric #-}

module Lib.Enemy.Types 
  ( EnemyType(..)
  , EnemyMovementType(..)
  ) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data EnemyType = 
  Firebug 
  | Leafbug 
  | MagmaCrab 
  | Scorpion 
  | Clampbeetle 
  | Firewasp 
  | FlyingLocust 
  | Voidbutterfly
  deriving (Show, Eq, Generic)

instance Hashable EnemyType

data EnemyMovementType = Walk | Fly
  deriving (Show, Eq)