module Types.Collider where

import Delude
import Engine.Common.Types

data CollideWith
   = CollideWith_High
   | CollideWith_Low
   deriving (Generic, Enum)

data BaseShapeDesc
   = BSD_Circle Float
   deriving (Generic)

data ShapeDesc
   = SD_Translate ShapeDescTranslate
   | SD_BaseShape BaseShapeDesc
   | SD_Composition (NonEmpty ShapeDesc)
   deriving (Generic)
data ShapeDescTranslate = ShapeDescTranslate
   { field_vector    :: (V2 Float)
   , field_shapeDesc :: ShapeDesc
   } deriving (Generic)

newtype ConvexPoly = ConvexPoly [V2 Float]
    deriving (Show)

data BaseShape
   = BS_Circle (V2 Float) Float

data Shape = Shape
    { field_parts :: (NonEmpty BaseShape)
    , field_bbox  :: BBox Float
    } deriving (Generic)

instance FromJSON CollideWith   where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON BaseShapeDesc where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON ShapeDesc     where parseJSON = genericParseJSON customOptionsJSON
instance FromJSON Shape         where parseJSON = fmap constructShape . parseJSON

instance FromJSON ShapeDescTranslate where
    parseJSON = genericParseJSON customOptionsJSON

--------------------------------------------------------------------------------

constructShape :: ShapeDesc -> Shape
constructShape desc = Shape ps bb
    where
    ps = go desc
    bb = bboxUnion $ fmap baseShapeBBox ps

    go :: ShapeDesc -> NonEmpty BaseShape
    go x = case x of
        SD_Translate   sdt -> trans sdt
        SD_BaseShape   bsd -> one $ toBaseShape bsd
        SD_Composition sds -> sconcat $ fmap go sds

    trans (ShapeDescTranslate v d) = overBaseShapeVec (+v) <$> go d
    toBaseShape = \case
        BSD_Circle r -> BS_Circle 0 r

overBaseShapeVec :: (V2 Float -> V2 Float) -> BaseShape -> BaseShape
overBaseShapeVec f = \case
    BS_Circle c r -> BS_Circle (f c) r

baseShapeBBox :: BaseShape -> BBox Float
baseShapeBBox = \case
    BS_Circle v r -> mkBBoxCenter v (pure $ r*2)

