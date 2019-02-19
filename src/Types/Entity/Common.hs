-- {-# Language ConstraintKinds #-}
{-# Language TemplateHaskell #-}
module Types.Entity.Common
    ( module Types.Entity.Common
    ) where

import Delude
import qualified Prelude
import Text.Printf

-- import Linear ((^+^), (^*))

--------------------------------------------------------------------------------

data EntityKind
   = EntityKind_Tile
   | EntityKind_Static
   | EntityKind_Item
   | EntityKind_Dynamic
   deriving (Eq, Ord, Generic)
instance Hashable EntityKind

data EntityId = EntityId
   { entityId_unique :: Word64
   , entityId_offset :: Int
   }
   deriving (Eq, Ord, Generic)
instance Hashable EntityId
makeFieldsCustom ''EntityId

instance HasEntityId EntityId EntityId where entityId = id

instance Show EntityId where
    show (EntityId x _) = "EID:" <> show x

type V2D = V2 Double

-- type Basic a = (Generic a, Default a)

newtype Location = Location { unLocation :: V2D    }
    deriving (Generic, Default)
makeWrapped ''Location

instance Show Location where
    show (Location (V2 x y)) = printf "(Location x=%.2f y=%.2f)" x y

newtype Distance = Distance { unDistance :: Double }
    deriving (Generic, Default, Show)
makeWrapped ''Distance

newtype Velocity = Velocity { unVelocity :: V2D    }
    deriving (Generic, Default, Show)
makeWrapped ''Velocity

newtype Health   = Health   { unHealth   :: Int    }
    deriving (Generic, Default, Show)
makeWrapped ''Health

newtype Speed    = Speed    { unSpeed    :: Double }
    deriving (Generic, Default, Show)
makeWrapped ''Speed

newtype Volume   = Volume   { unVolume   :: Double }
    deriving (Generic, Default, Show, Num, Eq, Ord)
makeWrapped ''Volume

newtype Time     = Time     { unTime     :: Double }
    -- deriving (Generic, Default, Show)

--------------------------------------------------------------------------------

data EntityDebugFlags = EntityDebugFlags
   { entityDebugFlags_drawPickupRange :: Bool
   } deriving (Generic)
makeFieldsCustom ''EntityDebugFlags
instance Default EntityDebugFlags

--------------------------------------------------------------------------------

-- make volume in liters. litre = 1000 cm^3
volumeL :: Double -> Volume
volumeL = Volume

-- distance in metres
disM :: Double -> Distance
disM = Distance

-- make location in meters.
locM :: Double -> Double -> Location
locM x y = Location $ V2 x y

locationInMeters :: V2D -> Location
locationInMeters = Location

velocityInMetersPerSecond :: V2D -> Velocity
velocityInMetersPerSecond = Velocity

timeInSeconds :: Double -> Time
timeInSeconds = Time

defaultDelta :: Time
defaultDelta = timeInSeconds 0.01 -- 10ms = 0.01s

type family   Delta a :: *
type instance Delta a = a

speedInMetersPerSecond :: Double -> Speed
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

