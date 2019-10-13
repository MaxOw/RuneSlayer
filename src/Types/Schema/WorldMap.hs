{-# Language DerivingVia #-}
{-# Language StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types.Schema.WorldMap where

import Relude
import Linear (V2)
import Codec.Winery
import Data.Default
import Data.Vector
import Types.Entity.TileSet (TileSetName, TileRole, Edge, Corner, Cross)
import Types.Entity.Common (Location)
import Types.Entity.Passive (PassiveTypeName)
import Types.Entity.Agent   (AgentTypeName)

deriving via WineryProduct (V2 Float)      instance Serialise (V2 Float)
deriving via WineryProduct Location        instance Serialise Location
deriving via WineryProduct PassiveTypeName instance Serialise PassiveTypeName
deriving via WineryProduct AgentTypeName   instance Serialise AgentTypeName
deriving via WineryProduct TileSetName     instance Serialise TileSetName
deriving via WineryVariant Edge            instance Serialise Edge
deriving via WineryVariant Corner          instance Serialise Corner
deriving via WineryVariant Cross           instance Serialise Cross
deriving via WineryVariant TileRole        instance Serialise TileRole

data PlaceAt a = PlaceAt
   { field_typeId   :: Int
   , field_location :: Location
   }
   deriving Generic
   deriving Serialise via WineryRecord (PlaceAt a)

data PlaceTileAt = PlaceTileAt
   { field_tileSet  :: TileSetName
   , field_tileRole :: TileRole
   , field_location :: Location
   }
   deriving Generic
   deriving Serialise via WineryRecord PlaceTileAt

data WorldMap = WorldMap
   -- { passivesNames     :: Vector PassiveTypeName
   -- , agentsNames       :: Vector AgentTypeName
   -- , passives          :: Vector (PlaceAt PassiveTypeName)
   -- , agents            :: Vector (PlaceAt AgentTypeName)
   { field_tiles             :: Vector PlaceTileAt
   }
   deriving Generic
   deriving Serialise via WineryRecord WorldMap
instance Default WorldMap

