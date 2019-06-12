-- {-# Language ConstraintKinds #-}
{-# Language TemplateHaskell #-}
module Types.Entity.Common
    ( module Types.Entity.Common

    , CollisionShape
    ) where

import Delude
import Data.Collider (CollisionShape)
import qualified Prelude
import Text.Printf

-- import Linear ((^+^), (^*))

--------------------------------------------------------------------------------

data EntityStatus
   = EntityStatus_HostilesInRange
   deriving (Eq, Ord)

data EntityKind
   = EntityKind_Tile
   | EntityKind_Static
   | EntityKind_Item
   | EntityKind_Dynamic
   deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Hashable EntityKind

data EntityId = EntityId
   { field_unique :: Word64
   , field_offset :: Int
   } deriving (Eq, Ord, Generic)
instance Hashable EntityId
instance HasEntityId EntityId EntityId where entityId = id

instance Show EntityId where
    show (EntityId x _) = "EID:" <> show x

data EntityWithIdT a = EntityWithId
   { field_entityId :: EntityId
   , field_entity   :: a
   } deriving (Generic)
instance HasEntityId (EntityWithIdT a) EntityId
instance HasEntity   (EntityWithIdT a) a

--------------------------------------------------------------------------------

type V2D = V2 Float

-- type Basic a = (Generic a, Default a)

newtype Location = Location { unLocation :: V2D    }
    deriving (Generic, Default, ToJSON, FromJSON)
makeWrapped ''Location

instance Show Location where
    show (Location (V2 x y)) = printf "(Location x=%.2f y=%.2f)" x y

newtype Distance = Distance { unDistance :: Float }
    deriving (Generic, Default, Show, Num, Eq, Ord, ToJSON, FromJSON)
makeWrapped ''Distance

newtype Velocity = Velocity { unVelocity :: V2D    }
    deriving (Generic, Default, Show, Num)
makeWrapped ''Velocity

newtype Health   = Health   { unHealth   :: Int    }
    deriving (Generic, Default, Show, ToJSON, FromJSON)
makeWrapped ''Health

newtype AttackPower = AttackPower { unAttackPower :: Int    }
    deriving (Generic, Default, Show, ToJSON, FromJSON, Eq, Ord, Num)
makeWrapped ''AttackPower

newtype Speed    = Speed    { unSpeed    :: Float }
    deriving (Generic, Default, Show, ToJSON, FromJSON)
makeWrapped ''Speed

newtype Volume   = Volume   { unVolume   :: Float }
    deriving (Generic, Default, Show, Num, Eq, Ord, ToJSON, FromJSON)
makeWrapped ''Volume

-- newtype Time     = Time Float
-- makeWrapped ''Time
    -- deriving (Generic, Default, Show)

newtype Duration = Duration Float
    deriving (Generic, Default, Num, Fractional, Eq, Ord, ToJSON, FromJSON)
makeWrapped ''Duration

--------------------------------------------------------------------------------

data EntityDebugFlags = EntityDebugFlags
   { field_drawPickupRange :: Bool
   } deriving (Generic)

instance Default EntityDebugFlags

--------------------------------------------------------------------------------

-- make volume in liters. litre = 1000 cm^3
volumeL :: Float -> Volume
volumeL = Volume

distanceInMeters :: Float -> Distance
distanceInMeters = Distance

-- distance in metres
disM :: Float -> Distance
disM = Distance

-- make location in meters.
locM :: Float -> Float -> Location
locM x y = Location $ V2 x y

locationInMeters :: V2D -> Location
locationInMeters = Location

velocityInMetersPerSecond :: V2D -> Velocity
velocityInMetersPerSecond = Velocity

timeInSeconds :: Float -> Duration
timeInSeconds = Duration

defaultDelta :: Duration
defaultDelta = timeInSeconds 0.01 -- 10ms = 0.01s

type family   Delta a :: *
type instance Delta a = a

speedInMetersPerSecond :: Float -> Speed
speedInMetersPerSecond = Speed

--------------------------------------------------------------------------------

isWithinDistance :: Distance -> Location -> Location -> Bool
isWithinDistance d a b = distance (a^._Wrapped) (b^._Wrapped) <= (d^._Wrapped)

defaultPickupRange :: Distance
defaultPickupRange = disM 1.5

maxEffectSpawnDistance :: Distance
maxEffectSpawnDistance = disM 20

