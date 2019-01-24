{-# Language TemplateHaskell #-}
module Types.Entity.Wall where

import Delude

import Types.Entity.Common (Location, Health)

--------------------------------------------------------------------------------

data Wall = Wall
   { wallLocation :: Location
   , wallHealth   :: Health
   } deriving (Generic)
instance Default Wall
makeFields ''Wall
