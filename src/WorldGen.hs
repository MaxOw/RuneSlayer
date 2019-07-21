{-# Language DeriveFunctor #-}
module WorldGen
    ( WorldGenConfig, WorldGenOutput
    , overviewImage
    , generateWorld
    ) where

import Delude
import qualified Data.Vector as Vector
import Engine.Common.Types
import Types.Entity (Entity)
import Types.Entity.Common
import Types.Entity.TileSet
import Types.Entity.Passive
import Entity.Passive
import Entity.Tile (makeTile)
import EntityLike (toEntity)
import ResourceManager (Resources, lookupTileSet, lookupPassive)

import qualified Entity.TileSet as TileSet

--------------------------------------------------------------------------------

import Data.Array.Repa (Array, D, DIM2, Z(..), (:.)(..), Source)
import Data.Array.Repa.Repr.Vector (V)
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Specialised.Dim2 as Repa
import qualified Color
import qualified Data.Colour.Palette.ColorSet as Palette

import Random.Utils (runRandom, randomListSelect, uniformRange, randomDirection)
import Data.Hashable (hash)

import Types.WorldGen
import WorldGen.Utils

--------------------------------------------------------------------------------

generateWorld :: Resources -> WorldGenConfig -> WorldGenOutput
generateWorld rs conf = def
    & entities      .~ Vector.fromList (tiles <> statics <> items)
    & overviewImage .~ Just (imgToImage fullImg)

--------------------------------------------------------------------------------

    where
    Size ox oy = fmap (negate . floor) $ conf^.size ^/ 2
    pp = V2 ox oy

    seed = conf^.ff#seed

    fullImg = foldr genLayer baseImg prepLayers

    baseImg = boolToImg landColor baseColor baseMap

    baseMap = generateLandmass conf
    mkLayerBoolMap i = generateLayer conf i baseMap
    layerBoolMaps = map mkLayerBoolMap $ zipWith const [1..] layers
    genLayer (l,c) = colorImg c l

    layers        = conf^.ff#coveringLayers
    layerTilesets = layers^..traverse.ff#tileset.to (requireTileSet rs)
    prepLayers    = zip layerBoolMaps $ zipWith tsColor defColors layerTilesets
    statics       = concat $ zipWith (placeStatics rs seed pp) layers layerBoolMaps

    defColors = map Color.colourConvert Palette.infiniteWebColors

    tsColor d t = fromMaybe d $ Color.fromColorDesc =<< t^.color
    baseColor = tsColor Color.blue baseTileSet
    landColor = tsColor Color.peru landTileSet

    baseTileSet = requireTileSetM rs $ conf^.ff#baseTileSet
    landTileSet = requireTileSetM rs $ conf^.ff#baseLandTileSet

--------------------------------------------------------------------------------

    baseRoles :: RepaD2 [(TileRole, TileSet)]
    baseRoles = Repa.map f $ boolToRole baseMap
        where
        f :: Maybe TileRole -> [(TileRole, TileSet)]
        f Nothing              = [(TileRole_Full, baseTileSet)]
        f (Just TileRole_Full) = [(TileRole_Full, landTileSet)]
        f (Just r)             = [(TileRole_Full, landTileSet)
                                 ,(TileSet.complem r, baseTileSet)]
    layerRoles :: [RepaD2 [(TileRole, TileSet)]]
    layerRoles = zipWith (\rtr ts -> Repa.map (mkLayerRole ts) rtr)
        (map boolToRole layerBoolMaps)
        layerTilesets

    mkLayerRole :: TileSet -> Maybe TileRole -> [(TileRole, TileSet)]
    mkLayerRole _ Nothing  = []
    mkLayerRole t (Just r) = [(r, t)]

    outRoles :: RepaD2 [(TileRole, TileSet)]
    outRoles = foldr (Repa.zipWith (<>)) baseRoles layerRoles

    tiles = buildEntities pp $ downselectTileSets outRoles

    items = mapMaybe fromPassive $ conf^.ff#items.traverse

    fromPassive :: LocatedPassive -> Maybe Entity
    fromPassive lp = make <$> lookupPassive (lp^.name) rs
        where
        make tn = toEntity $ makePassive rs tn & location .~ (Just $ lp^.location)

--------------------------------------------------------------------------------

placeStatics
    :: Resources -> Int -> V2 Int -> CoveringLayer -> RepaBool -> [Entity]
placeStatics rs seed pp layer bm = mapMaybe (selectStatic rs seed sts) vsv
    where
    rnd = rndBool seed 20 (Repa.extent bm)
    psl :: Array V DIM2 (V2 Int, Bool)
    psl = Repa.computeS $ addOffset pp $ Repa.zipWith (&&) rnd $ shrinkBool bm
    vsv = map fst $ filter snd $ Repa.toList psl
    sts = map (requireStatic rs) $ layer^.ff#statics

selectStatic :: Resources -> Int -> [PassiveType] -> V2 Int -> Maybe Entity
selectStatic rs seed ss p
    | inClearance = Nothing
    | otherwise = flip (placeStatic rs) (pp+off) <$> s
    where
    inClearance = norm pp < 6
    pp = fmap fromIntegral p
    ps = xor seed (hash p)
    (s, off) = runRandom ps $ do
        ls <- randomListSelect ss
        d  <- randomDirection
        sc <- uniformRange (0, 0.4)
        return (ls, d^*sc)

downselectTileSets
    :: RepaD2 [(TileRole, TileSet)] -> RepaD2 [(TileRole, TileSet)]
downselectTileSets = Repa.map f
    where
    f :: [(TileRole, TileSet)] -> [(TileRole, TileSet)]
    f is = case topz of
        Nothing -> is
        Just tz -> filter (\x -> x^._2.zindex >= tz^._2.zindex) is
        where
        fulls = filter (\x -> fst x == TileRole_Full) is
        topz = viaNonEmpty head $ sortOn (view (_2.zindex)) fulls

buildEntities :: V2 Int -> RepaD2 [(TileRole, TileSet)] -> [Entity]
buildEntities pp = concat . Repa.toList . Repa.map f . addOffset pp
    where
    f (v, ls) = map (\(tr, ts) -> mkLocRole ts v tr) ls

--------------------------------------------------------------------------------

requireTileSetM :: Resources -> Maybe TileSetName -> TileSet
requireTileSetM rs mn = require "Unable to load tile set." $
    flip lookupTileSet rs =<< mn

requireTileSet :: Resources -> TileSetName -> TileSet
requireTileSet rs n = require "Unable to load tile set." $
    lookupTileSet n rs

requireStatic :: Resources -> PassiveTypeName -> PassiveType
requireStatic rs n = require "Unable to load static entity." $
    lookupPassive n rs

generateLandmass :: WorldGenConfig -> RepaBool
generateLandmass conf
    = Repa.map not $ Repa.zipWith (&&)
    (circleCutoff sh $ Repa.zipWith (*)
        (simplePerlinNoise (conf^.ff#seed) sh)
        (simplePerlinNoise (conf^.ff#seed+1) sh))
    (circleCutoff sh $ simplePerlinNoise (conf^.ff#seed+3) sh)
    where
    sh = (Z :. w :. h)
    Size w h = fmap floor $ conf^.size

generateLayer :: WorldGenConfig -> Int -> RepaBool -> RepaBool
generateLayer conf i b
    = Repa.zipWith (&&) b
    $ circleCutoff sh $ simplePerlinNoise (conf^.ff#seed+i+10) sh
    where
    sh = (Z :. w :. h)
    Size w h = fmap floor $ conf^.size

--------------------------------------------------------------------------------

placeStatic :: Resources -> PassiveType -> V2 Float -> Entity
placeStatic rs se (V2 x y) = toEntity $ makePassive rs se
    & location .~ Just (locM x y)

addOffset :: Source x a => V2 Int -> Array x DIM2 a -> Array D DIM2 (V2 Int, a)
addOffset off s = Repa.traverse s id f
    where f g c@(Z :. x :. y) = (off + V2 x y, g c)

boolToRole :: Source x Bool => Array x DIM2 Bool -> Array D DIM2 (Maybe TileRole)
boolToRole s = Repa.traverse s id f
    where
    es = Repa.extent s
    f g (Z :. x :. y) = TileSet.toRole atI
        where atI (a,b) = g $ clamp2 es (a+x, b+y)

clamp2 :: DIM2 -> (Int, Int) -> DIM2
clamp2 es (a, b) = Repa.clampToBorder2 es $ Repa.ix2 a b

--------------------------------------------------------------------------------

mkLocRole :: TileSet -> V2 Int -> TileRole -> Entity
mkLocRole ts pos r = toEntity . set location loc $ makeTile r ts
    where
    loc = Location $ fmap fromIntegral pos

