{-# Language DeriveFunctor #-}
module WorldGen where

import Delude
import qualified Relude.Extra.Enum as Enum
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Engine.Types (Engine)
import Engine.Common.Types
import Types.Entity (Entity)
import Types.Entity.Common
import Types.Entity.TileSet
import Types.Entity.StaticEntity
import Entity.StaticEntity
import Entity.Tile (makeTile)
import EntityLike (toEntity)
import ResourceManager (Resources, lookupTileSet, lookupStaticEntity)

import qualified Entity.TileSet as TileSet

--------------------------------------------------------------------------------

import Data.Array.Repa (Array, U, D, DIM2, Z(..), (:.)(..), Source)
import Data.Array.Repa.Repr.Vector (V)
-- import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Array.Repa.Algorithms.Randomish as Repa
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Specialised.Dim2 as Repa

--------------------------------------------------------------------------------

generateWorld :: Resources -> Size Float -> Engine us (Vector Entity)
generateWorld rs worldSize = do
    -- mapM_ print offM33
    let Size ox oy = fmap (negate . floor) $ worldSize ^/ 2
    let Size w  h  = fmap floor worldSize
    -- let ps = [V2 (ox+x) (oy+y) | x <- [0..w], y <- [0..h]]
    -- return $ Vector.fromList $ map mkGrass ps
    -- return $ Vector.fromList $ catMaybes $ zipWith mkRole ps allRoles
    -- return $ Vector.fromList $ take 10 $ zipWith mkBool ps $ cycle [True, False]
    -- return $ Vector.fromList $ concat $ take 51 $ zipWith mk3x3 ps [0..]
    let pp = V2 ox oy
    let dirtTileSet  = lookupTileSet (TileSetName "Dry Dirt") rs
    let grassTileSet = lookupTileSet (TileSetName "Water")    rs
    let tiles = case (dirtTileSet, grassTileSet) of
            (Just dts, Just gts) ->
                concatMap (mkRoleL dts gts pp) $ rndRoleGrid 18 (V2 w h)
            _ -> []
    -- let se = testStaticEntityType_tree
    let mse = lookupStaticEntity (StaticEntityTypeName "Tree") rs
    let trees = case mse of
            Nothing -> []
            Just se -> map (placeStatic se pp) $ rndTreeGrid 18 (V2 w h)
    return $ Vector.fromList $ tiles <> trees

--------------------------------------------------------------------------------

rndBool :: Int -> Int -> DIM2 -> Array D DIM2 Bool
rndBool seed rng sh = intToBool $ Repa.randomishIntArray sh 0 rng seed

intToBool :: Source x Int => Array x DIM2 Int -> Array D DIM2 Bool
intToBool = Repa.map (== 0)

testArray :: Array U DIM2 Bool
testArray = Repa.fromListUnboxed (Z :. 3 :. 3) $ replicate (3*3) False

rndRoleGrid :: Int -> V2 Int -> [(V2 Int, [TileRole])]
rndRoleGrid seed (V2 x y) = Repa.toList imtr
    where
    imtr :: Array V DIM2 (V2 Int, [TileRole])
    imtr = Repa.computeS $ makeRoleGrid $ rndBool seed 2 (Z :. x :. y)

rndTreeGrid :: Int -> V2 Int -> [V2 Int]
rndTreeGrid seed (V2 x y) = map fst $ filter snd $ Repa.toList imtr
    where
    grassGrid = rndBool seed 2 (Z :. x :. y)
    topGrid   = rndBool 8 (seed+1) (Z :. x :. y)
    andGrid   = Repa.zipWith (&&) grassGrid topGrid
    imtr :: Array V DIM2 (V2 Int, Bool)
    imtr = Repa.computeS $ addOffset andGrid

placeStatic :: StaticEntityType -> V2 Int -> V2 Int -> Entity
placeStatic se pp p = toEntity $ makeStaticEntity se
    & location .~ locM x y
    where
    V2 x y = fmap fromIntegral (pp + p)

makeRoleGrid :: Source x Bool
    => Array x DIM2 Bool -> Array D DIM2 (V2 Int, [TileRole])
makeRoleGrid = addOffset . boolToRole

addOffset :: Source x a => Array x DIM2 a -> Array D DIM2 (V2 Int, a)
addOffset s = Repa.traverse s id f
    where f g c@(Z :. x :. y) = (V2 x y, g c)

boolToRole :: Source x Bool => Array x DIM2 Bool -> Array D DIM2 [TileRole]
boolToRole s = Repa.traverse s id f
    where
    es = Repa.extent s
    f g (Z :. x :. y) = TileSet.toRole atI
        where atI (a,b) = g $ clamp2 es (a+x, b+y)

clamp2 :: DIM2 -> (Int, Int) -> DIM2
clamp2 es (a, b) = Repa.clampToBorder2 es $ Repa.ix2 a b

--------------------------------------------------------------------------------

mkRoleL :: TileSet -> TileSet -> V2 Int -> (V2 Int, [TileRole]) -> [Entity]
mkRoleL dts gts pp (pos, mr) = condAddDirt $ map (mkLocRole gts $ pp+pos) mr
    where
    isFull = any (== TileRole_Full) mr
    condAddDirt = if isFull then id
        else (fullTile dts (pp+pos):)

fullTile :: TileSet -> V2 Int -> Entity
fullTile ts p = toEntity . set location loc $ makeTile TileRole_Full ts
    where
    loc = Location $ fmap fromIntegral p

mkLocRole :: TileSet -> V2 Int -> TileRole -> Entity
mkLocRole ts pos r = toEntity . set location loc $ makeTile r ts
    where
    loc = Location $ fmap fromIntegral pos

