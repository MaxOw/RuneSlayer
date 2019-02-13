{-# Language TemplateHaskell #-}
module Types.Entity.Wall where

import Delude

import Types.Entity.Common (Location, Health)

--------------------------------------------------------------------------------

data Wall = Wall
   { wall_location :: Location
   , wall_health   :: Health
   } deriving (Generic)
instance Default Wall
makeFieldsCustom ''Wall
