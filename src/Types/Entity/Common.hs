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

data EntityKind
   = EntityKind_Tile
   | EntityKind_Static
   | EntityKind_Item
   | EntityKind_Dynamic
   deriving (Eq, Ord, Enum, Bounded, Generic)
instance Hashable EntityKind

data EntityId = EntityId
   { field_unique :: Word64
   , field_offset :: Int
   } deriving (Eq, Ord, Generic)
instance Hashable EntityId


instance HasEntityId EntityId EntityId where entityId = id

instance Show EntityId where
    show (EntityId x _) = "EID:" <> show x

type V2D = V2 Float

-- type Basic a = (Generic a, Default a)

newtype Location = Location { unLocation :: V2D    }
    deriving (Generic, Default, ToJSON, FromJSON)
makeWrapped ''Location

instance Show Location where
    show (Location (V2 x y)) = printf "(Location x=%.2f y=%.2f)" x y

newtype Distance = Distance { unDistance :: Float }
    deriving (Generic, Default, Show)
makeWrapped ''Distance

newtype Velocity = Velocity { unVelocity :: V2D    }
    deriving (Generic, Default, Show)
makeWrapped ''Velocity

newtype Health   = Health   { unHealth   :: Int    }
    deriving (Generic, Default, Show, ToJSON, FromJSON)
makeWrapped ''Health

newtype AttackPower = AttackPower { unAttackPower :: Int    }
    deriving (Generic, Default, Show)
makeWrapped ''AttackPower

newtype Speed    = Speed    { unSpeed    :: Float }
    deriving (Generic, Default, Show)
makeWrapped ''Speed

newtype Volume   = Volume   { unVolume   :: Float }
    deriving (Generic, Default, Show, Num, Eq, Ord, ToJSON, FromJSON)
makeWrapped ''Volume

newtype Time     = Time     { unTime     :: Float }
makeWrapped ''Time
    -- deriving (Generic, Default, Show)

--------------------------------------------------------------------------------

data EntityDebugFlags = EntityDebugFlags
   { field_drawPickupRange :: Bool
   } deriving (Generic)

instance Default EntityDebugFlags

--------------------------------------------------------------------------------

-- make volume in liters. litre = 1000 cm^3
volumeL :: Float -> Volume
volumeL = Volume

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

timeInSeconds :: Float -> Time
timeInSeconds = Time

defaultDelta :: Time
defaultDelta = timeInSeconds 0.01 -- 10ms = 0.01s

type family   Delta a :: *
type instance Delta a = a

speedInMetersPerSecond :: Float -> Speed
speedInMetersPerSecond = Speed

--------------------------------------------------------------------------------

baseWalkingSpeed :: Speed
baseWalkingSpeed = speedInMetersPerSecond 2

baseRunningSpeed :: Speed
baseRunningSpeed = speedInMetersPerSecond 6

baseSprintingSpeed :: Speed
baseSprintingSpeed = speedInMetersPerSecond 8

--------------------------------------------------------------------------------

isWithinDistance :: Distance -> Location -> Location -> Bool
isWithinDistance d a b = distance (a^._Wrapped) (b^._Wrapped) <= (d^._Wrapped)

defaultPickupRange :: Distance
defaultPickupRange = disM 1.5

