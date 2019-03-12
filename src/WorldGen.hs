{-# Language TemplateHaskell #-}
{-# Language DeriveFunctor #-}
module WorldGen where

import Delude
import qualified Relude.Extra.Enum as Enum
-- import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Engine.Types (Engine)
import Engine.Common.Types
import Types.Entity (Entity)
import Types.Entity.Common
import Types.Entity.TileType
import Entity.StaticEntity
import Entity.Tile (makeSimpleTile, makeSimpleFullTile)
import EntityLike (toEntity)
import qualified Resource
import Resource (Resource)
-- import Data.Grid (Grid, Coord(..))
-- import qualified Data.Grid as Grid
--import Data.Reflection

--------------------------------------------------------------------------------

import Data.Array.Repa (Array, U, D, DIM2, Z(..), (:.)(..), Source)
import Data.Array.Repa.Repr.Vector (V)
-- import Data.Array.Repa.Repr.Unboxed (Unbox)
import qualified Data.Array.Repa.Algorithms.Randomish as Repa
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Specialised.Dim2 as Repa

--------------------------------------------------------------------------------

data CornerQuad a = CornerQuad
   { cornerQuad_bottomRight :: a
   , cornerQuad_bottomLeft  :: a
   , cornerQuad_topLeft     :: a
   , cornerQuad_topRight    :: a
   } deriving (Functor)
makeFieldsCustom ''CornerQuad

type Meter = Float

generateWorld :: Size Meter -> Engine us (Vector Entity)
generateWorld worldSize = do
    -- mapM_ print offM33
    let Size ox oy = fmap (negate . floor) $ worldSize ^/ 2
    let Size w  h  = fmap floor worldSize
    -- let ps = [V2 (ox+x) (oy+y) | x <- [0..w], y <- [0..h]]
    -- return $ Vector.fromList $ map mkGrass ps
    -- return $ Vector.fromList $ catMaybes $ zipWith mkRole ps allRoles
    -- return $ Vector.fromList $ take 10 $ zipWith mkBool ps $ cycle [True, False]
    -- return $ Vector.fromList $ concat $ take 51 $ zipWith mk3x3 ps [0..]
    let pp = V2 ox oy
    let tiles = concatMap (mkRoleL pp) $ rndRoleGrid 18 (V2 w h)
    let trees = map (placeTree pp) $ rndTreeGrid 18 (V2 w h)
    return $ Vector.fromList $ tiles <> trees

--------------------------------------------------------------------------------

{-
-- partCell :: Array x DIM2 a -> Array D DIM2 (Array y DIM2 a)
-- partCell :: Source x a => Array x DIM2 a -> Array D DIM2 (Array y DIM2 a)
partCell :: Unbox a => Array U DIM2 a -> Array D DIM2 (Array U DIM2 a)
partCell s = Repa.traverse s e f
    where
    nx = Z :. 2 :. 2
    e _ = nx
    f g (Z :. x :. y) = Repa.fromListUnboxed nx $
        [ g (Z :. x   :. y)
        , g (Z :. x+1 :. y)
        , g (Z :. x   :. y+1)
        , g (Z :. x+1 :. y+1)
        ]
-}

rndBool :: Int -> Int -> DIM2 -> Array D DIM2 Bool
rndBool seed rng sh = intToBool $ Repa.randomishIntArray sh 0 rng seed
    -- where seed = 4

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

placeTree :: V2 Int -> V2 Int -> Entity
placeTree pp p = toEntity $ makeStaticEntity testStaticEntityType_tree
    & location .~ locM x y
    where
    V2 x y = fmap fromIntegral (pp + p)

worldGenTest :: IO ()
worldGenTest = do
    return ()

flipEdge :: Edge -> Edge
flipEdge = \case
   Edge_Bottom -> Edge_Top
   Edge_Left   -> Edge_Right
   Edge_Top    -> Edge_Bottom
   Edge_Right  -> Edge_Left

flipCorner :: Corner -> Corner
flipCorner = \case
   Corner_BottomRight -> Corner_TopLeft
   Corner_BottomLeft  -> Corner_TopRight
   Corner_TopLeft     -> Corner_BottomRight
   Corner_TopRight    -> Corner_BottomLeft

flipRole :: Maybe TileRole -> Maybe TileRole
flipRole Nothing              = Just TileRole_Full
flipRole (Just TileRole_Full) = Nothing
flipRole (Just r) = Just $ case r of
   TileRole_Full          -> TileRole_Full
   TileRole_Path          -> TileRole_Hole
   TileRole_Hole          -> TileRole_Path
   TileRole_Edge        e -> TileRole_Edge        $ flipEdge   e
   TileRole_OuterCorner c -> TileRole_InnerCorner c -- $ flipCorner c
   TileRole_InnerCorner c -> TileRole_OuterCorner c -- $ flipCorner c

flipRoles :: [TileRole] -> [TileRole]
flipRoles [] = [TileRole_Full]
flipRoles os = mapMaybe (flipRole . Just) os

makeRoleGrid :: Source x Bool
    => Array x DIM2 Bool -> Array D DIM2 (V2 Int, [TileRole])
-- makeRoleGrid = addOffset . Repa.map flipRoles . boolToRole . fixupGrid . fixupGrid
makeRoleGrid = addOffset . boolToRole . fixupGrid . fixupGrid

addOffset :: Source x a => Array x DIM2 a -> Array D DIM2 (V2 Int, a)
addOffset s = Repa.traverse s id f
    where f g c@(Z :. x :. y) = (V2 x y, g c)

fixupGrid :: Source x Bool => Array x DIM2 Bool -> Array D DIM2 Bool
fixupGrid s = Repa.traverse s id f
    where
    es@(Z :. w :. h) = Repa.extent s
    f g (Z :. x :. y)
        | isEdge = False
        | atI ( 0, 0)                  ||
         (atI (-1, 0) &&  atI ( 1, 0)) ||
         (atI ( 0,-1) &&  atI ( 0, 1)) -- ||
        = True
        | otherwise = False
        where
        atI (a,b) = g $ clamp2 es (a+x, b+y)
        isEdge = x==0 || y==0 || x==(w-1) || y==(h-1)

boolToRole :: Source x Bool => Array x DIM2 Bool -> Array D DIM2 [TileRole]
boolToRole s = Repa.traverse s id f
    where
    es = Repa.extent s
    f g (Z :. x :. y)
        | atI ( 0, 0)                = [TileRole_Full]

        | atI ( 1, 0) && atI (-1,-1) && atI ( 0, 1) = [TileRole_Hole]
        | atI (-1, 0) && atI ( 1, 1) && atI ( 0,-1) = [TileRole_Hole]
        | atI ( 0, 1) && atI ( 1,-1) && atI (-1, 0) = [TileRole_Hole]
        | atI ( 0,-1) && atI (-1, 1) && atI ( 1, 0) = [TileRole_Hole]

        | atI ( 1, 0) && atI ( 0, 1) = [TileRole_InnerCorner Corner_TopRight]
        | atI (-1, 0) && atI ( 0, 1) = [TileRole_InnerCorner Corner_TopLeft]
        | atI ( 1, 0) && atI ( 0,-1) = [TileRole_InnerCorner Corner_BottomRight]
        | atI (-1, 0) && atI ( 0,-1) = [TileRole_InnerCorner Corner_BottomLeft]

        | atI ( 1, 0) && atI (-1,-1) = [TileRole_InnerCorner Corner_BottomRight]
        | atI (-1, 0) && atI ( 1, 1) = [TileRole_InnerCorner Corner_TopLeft]
        | atI ( 0, 1) && atI ( 1,-1) = [TileRole_InnerCorner Corner_TopRight]
        | atI ( 0,-1) && atI (-1, 1) = [TileRole_InnerCorner Corner_BottomLeft]

        | atI ( 1, 0) && atI (-1, 1) = [TileRole_InnerCorner Corner_TopRight]
        | atI (-1, 0) && atI ( 1,-1) = [TileRole_InnerCorner Corner_BottomLeft]
        | atI ( 0, 1) && atI (-1,-1) = [TileRole_InnerCorner Corner_TopLeft]
        | atI ( 0,-1) && atI ( 1, 1) = [TileRole_InnerCorner Corner_BottomRight]

        | atI ( 1, 0)                = [TileRole_Edge Edge_Left]
        | atI (-1, 0)                = [TileRole_Edge Edge_Right]
        | atI ( 0, 1)                = [TileRole_Edge Edge_Bottom]
        | atI ( 0,-1)                = [TileRole_Edge Edge_Top]

        | otherwise
            = ifI ( 1, 1) (TileRole_OuterCorner Corner_BottomLeft)
            $ ifI (-1, 1) (TileRole_OuterCorner Corner_BottomRight)
            $ ifI ( 1,-1) (TileRole_OuterCorner Corner_TopLeft)
            $ ifI (-1,-1) (TileRole_OuterCorner Corner_TopRight)
            $ []

        where
        ifI i v = if atI i then (v:) else id
        atI (a,b) = g $ clamp2 es (a+x, b+y)

clamp2 :: DIM2 -> (Int, Int) -> DIM2
clamp2 es (a, b) = Repa.clampToBorder2 es $ Repa.ix2 a b

--------------------------------------------------------------------------------

pureCornerQuad :: x -> CornerQuad x
pureCornerQuad x = CornerQuad x x x x

mkGrass :: V2 Int -> Entity
mkGrass pos@(V2 x y) = mkLocEnt pos tile
    where
    p0 = 2147480011
    p1 = 2147480197
    tile = mkTile $ mod (x*p0 + y*p1 + x*y*p0*p1 + x + y*x) 3
    mkTile i = Resource.mkEnvRect (42 + i*2) 10 2 2

mkRoleL :: V2 Int -> (V2 Int, [TileRole]) -> [Entity]
mkRoleL pp (pos, mr) = condAddDirt $ mapMaybe (mkRole $ pp+pos) mr
    where
    isFull = any (== TileRole_Full) mr
    condAddDirt = if isFull then id else (dirtTile (pp+pos):)

dirtTile :: V2 Int -> Entity
dirtTile p = toEntity . set location loc . makeSimpleFullTile
    $ Resource.mkEnvRect 38 10 2 2
    where
    loc = Location $ fmap fromIntegral p

mkRole :: V2 Int -> TileRole -> Maybe Entity
mkRole pos r = mkLocEnt pos <$> roleToOffset r

mkLocEnt :: V2 Int -> Resource -> Entity
mkLocEnt pos = toEntity . set location loc . makeSimpleTile
    where
    loc = Location $ fmap fromIntegral pos

mkBool :: V2 Int -> Bool -> Entity
mkBool p b = mkLocEnt p $ boolToRes b
    where
    boolToRes True  = Resource.mkEnvRect 42 10 2 2
    boolToRes False = Resource.mkEnvRect 54 10 2 2

{-
mk3x3 :: V2 Int -> Int -> [Entity]
mk3x3 (V2 x y) i = c -- [mkBool pp True]
    where
    pp = V2 (x-4) (y*7)
    m = indexUniqueSym33 i
    -- c = map (\(p,b) -> mkBool (p+pp) b) $ concatM33 $ combM33 (,) offM33 m
    c = mk3x3Quad pp m -- $ concatM33 $ combM33 (,) offM33 m

mk3x3Quad :: V2 Int -> M33 Bool -> [Entity]
mk3x3Quad p = concatMap (mkTileQuad p) . gridToIndexedRoles . m33toGrid
-}

mkTileQuad :: V2 Int -> (V2 Int, CornerQuad (Maybe TileRole)) -> [Entity]
mkTileQuad offp (pos, q) = catMaybes
    [ mkLocEnt pBL <$> (roleToOffset =<< q^.bottomLeft)
    , mkLocEnt pBR <$> (roleToOffset =<< q^.bottomRight)
    , mkLocEnt pTL <$> (roleToOffset =<< q^.topLeft)
    , mkLocEnt pTR <$> (roleToOffset =<< q^.topRight)
    ]
    where
    pBL = pp & _xy -~ 1 -- &  _y -~ 1
    pBR = pp &  _y -~ 1
    pTL = pp & _x  -~ 1
    pTR = pp -- & _xy -~ 0 -- &  _y -~ 1
    pp  =  ((pos & _y %~ negate) ^* 2) + offp


genTest :: MonadIO m => m ()
genTest = do
    -- print $ length allM33
    -- print $ length uniqueSym33
    return ()

-- generateContinents

allRoles :: [TileRole]
allRoles =
   [ TileRole_Full
   , TileRole_Path
   , TileRole_Hole ]
   <> fmap TileRole_Edge        boundedRange
   <> fmap TileRole_OuterCorner boundedRange
   <> fmap TileRole_InnerCorner boundedRange

roleToOffset :: TileRole -> Maybe Resource
roleToOffset = \case
    TileRole_Full          -> mkr 1 5
    TileRole_Path          -> mkr 0 0
    -- TileRole_Hole          -> Nothing
    TileRole_Hole          -> mkr 1 5
    TileRole_Edge        e -> mkE  e
    TileRole_OuterCorner c -> mkOC c
    TileRole_InnerCorner c -> mkIC c
    where
    mkr a b = Just $ Resource.mkEnvRect (42+a*2) (b*2) 2 2
    mkE = \case
        Edge_Top    -> mkr 1 2
        Edge_Left   -> mkr 0 3
        Edge_Right  -> mkr 2 3
        Edge_Bottom -> mkr 1 4
    mkOC = \case
        Corner_TopLeft     -> mkr 0 2
        Corner_TopRight    -> mkr 2 2
        Corner_BottomLeft  -> mkr 0 4
        Corner_BottomRight -> mkr 2 4
    mkIC = \case
        Corner_TopLeft     -> mkr 1 0
        Corner_TopRight    -> mkr 2 0
        Corner_BottomLeft  -> mkr 1 1
        Corner_BottomRight -> mkr 2 1

{-
coordToV2 :: Coord [a, b] -> V2 Int
coordToV2 = \case
    Coord [x, y] -> V2 y x
    _            -> error "Impossible happend!"

-}

{-
reifyTabulateGrid
    :: (Integer, Integer)
    -> (Coord [w,h] -> a)
    -> Grid [w,h] a
reifyTabulateGrid (w,h) f =
    reifyNat w $ \pw ->
    reifyNat h $ \ph ->
    proxyTabulateGrid (pw, ph) f
-}

{-
m33toGrid :: M33 a -> Grid [3, 3] a
m33toGrid = Grid.fromNestedLists' . map toList . toList

proxyTabulateGrid
    :: KnownNat w
    => KnownNat h
    => (Proxy w, Proxy h)
    -> (Coord [w,h] -> a)
    -> Grid [w,h] a
proxyTabulateGrid _ f = Grid.tabulate f

gridToIndexedRoles
    :: KnownNat w
    => KnownNat h
    => Grid [w,h] Bool -> [(V2 Int, CornerQuad (Maybe TileRole))]
gridToIndexedRoles =
    map (over _1 coordToV2) . toList . indexedGrid . gridToRoles

indexedGrid :: Grid.Dimensions d => Grid d a -> Grid d (Coord d, a)
indexedGrid g = (,) <$> cs <*> g
    where cs = Grid.tabulate id

gridToRoles :: Grid.Dimensions d
    => Grid d Bool -> Grid d (CornerQuad (Maybe TileRole))
gridToRoles = Grid.autoConvolute Grid.clampWindow kernelToRoleQuad

cornerToRole_BR :: Grid [2,2] Bool -> Maybe TileRole
cornerToRole_BR g = case Grid.toNestedLists g of
    [ [_, r] ,
      [b, c] ] -> if
        | b && r     -> Just $ TileRole_InnerCorner Corner_BottomRight
        | b && not r -> Just $ TileRole_Edge          Edge_Top
        | r && not b -> Just $ TileRole_Edge          Edge_Left
        | c          -> Just $ TileRole_OuterCorner Corner_TopLeft
        | otherwise  -> Nothing
    _ -> Nothing

cornerToRole_BL :: Grid [2,2] Bool -> Maybe TileRole
cornerToRole_BL g = case Grid.toNestedLists g of
    [ [l, _] ,
      [c, b] ] -> if
        | b && l     -> Just $ TileRole_InnerCorner Corner_BottomLeft
        | b && not l -> Just $ TileRole_Edge          Edge_Top
        | l && not b -> Just $ TileRole_Edge          Edge_Right
        | c          -> Just $ TileRole_OuterCorner Corner_TopRight
        | otherwise  -> Nothing
    _ -> Nothing

cornerToRole_TL :: Grid [2,2] Bool -> Maybe TileRole
cornerToRole_TL g = case Grid.toNestedLists g of
    [ [c, t] ,
      [l, _] ] -> if
        | t && l     -> Just $ TileRole_InnerCorner Corner_TopLeft
        | t && not l -> Just $ TileRole_Edge          Edge_Bottom
        | l && not t -> Just $ TileRole_Edge          Edge_Right
        | c          -> Just $ TileRole_OuterCorner Corner_BottomRight
        | otherwise  -> Nothing
    _ -> Nothing

cornerToRole_TR :: Grid [2,2] Bool -> Maybe TileRole
cornerToRole_TR g = case Grid.toNestedLists g of
    [ [t, c] ,
      [_, r] ] -> if
        | t && r     -> Just $ TileRole_InnerCorner Corner_TopRight
        | t && not r -> Just $ TileRole_Edge          Edge_Bottom
        | r && not t -> Just $ TileRole_Edge          Edge_Left
        | c          -> Just $ TileRole_OuterCorner Corner_BottomLeft
        | otherwise  -> Nothing
    _ -> Nothing
-}


class RotateMod4 x where
    rotateMod4 :: x -> x

rotateMod4N :: RotateMod4 x => Int -> x -> x
rotateMod4N i = case mod i 4 of
    1 -> f
    2 -> f.f
    3 -> f.f.f
    _ -> id
    where f = rotateMod4

{-
partitionCell :: Grid [3, 3] a -> CornerQuad (Grid [2, 2] a)
partitionCell x = case Grid.toNestedLists x of
    [ [a, b, c] ,
      [d, e, f] ,
      [g, h, i] ] -> CornerQuad
        { cornerQuad_topLeft     = Grid.fromNestedLists' [ [a, b], [d, e] ]
        , cornerQuad_topRight    = Grid.fromNestedLists' [ [b, c], [e, f] ]
        , cornerQuad_bottomRight = Grid.fromNestedLists' [ [e, f], [h, i] ]
        , cornerQuad_bottomLeft  = Grid.fromNestedLists' [ [d, e], [g, h] ] }
    _ -> error "This shouldn't be possible!"

kernelToRoleQuad :: Grid [3, 3] Bool -> CornerQuad (Maybe TileRole)
kernelToRoleQuad k
    | Grid.index k (Coord [1,1]) == True = pureCornerQuad (Just TileRole_Full)
    | otherwise = CornerQuad
    { cornerQuad_topLeft     = cornerToRole_TL $ kk^.topLeft
    , cornerQuad_topRight    = cornerToRole_TR $ kk^.topRight
    , cornerQuad_bottomRight = cornerToRole_BR $ kk^.bottomRight
    , cornerQuad_bottomLeft  = cornerToRole_BL $ kk^.bottomLeft
    }
    where
    kk = partitionCell k
    -- underRotToRole i
        -- = cornerToRole
        -- = fmap (rotateMod4N i)
        -- . cornerToRole
        -- . rotateMod4N_CCW i
-}

rotateMod4_CCW :: RotateMod4 x => x -> x
rotateMod4_CCW = rotateMod4N 3

rotateMod4N_CCW :: RotateMod4 x => Int -> x -> x
rotateMod4N_CCW i = rotateMod4N (4 - mod i 4)

{-
instance RotateMod4 (Grid [2, 2] x) where
    rotateMod4 g = case Grid.toNestedLists g of
        [ [a, b] , [c, d] ] -> Grid.fromNestedLists' [ [c, a] , [d, b] ]
        _ -> g
-}

instance RotateMod4 (M22 x) where
    rotateMod4
        (V2 (V2 m00 m01)
            (V2 m10 m11))
        =
        (V2 (V2 m10 m00)
            (V2 m11 m01))

instance RotateMod4 TileRole where
    rotateMod4 (TileRole_InnerCorner x) = TileRole_InnerCorner $ Enum.next x
    rotateMod4 (TileRole_OuterCorner x) = TileRole_OuterCorner $ Enum.next x
    rotateMod4 (TileRole_Edge        x) = TileRole_Edge        $ Enum.next x
    rotateMod4 other                    = other

--------------------------------------------------------------------------------

offM33 :: M33 (V2 Int)
offM33 = (\y -> fmap (flip V2 y) vv) <$> vv^._zyx
    where vv = V3 (-1) 0 1

combM33 :: (a -> b -> c) -> M33 a -> M33 b -> M33 c
combM33 f a b = liftA2 (liftA2 f) a b

concatM33 :: M33 a -> [a]
concatM33 = concatMap toList . fmap toList

allM33 :: [M33 Bool]
allM33 = V3 <$> vs <*> vs <*> vs where
    vs = V3 <$> tf <*> tf <*> tf
    tf = [True, False]

uniqueSym33 :: [Sym33 Bool]
uniqueSym33 = hashNub $ map mkSym33 allM33

uniqueM33F :: [M33 Bool]
uniqueM33F = filter (not . view (_y._y)) $ map (fromSym33 False) uniqueSym33

indexUniqueSym33 :: Int -> M33 Bool
indexUniqueSym33 i = fromMaybe (pure $ pure False)
    $ preview (ix i) uniqueM33F

fromSym33 :: a -> Sym33 a -> M33 a
fromSym33 d = fromMaybe (pure $ pure d) . viaNonEmpty head . unSym33

rotM33 :: M33 a -> M33 a
rotM33
    (V3 (V3 m00 m01 m02)
        (V3 m10 m11 m12)
        (V3 m20 m21 m22))
    =
    (V3 (V3 m20 m10 m00)
        (V3 m21 m11 m01)
        (V3 m22 m12 m02))

mirM33 :: M33 a -> M33 a
mirM33
    (V3 (V3 m00 m01 m02)
        (V3 m10 m11 m12)
        (V3 m20 m21 m22))
    =
    (V3 (V3 m02 m01 m00)
        (V3 m12 m11 m10)
        (V3 m22 m21 m20))

newtype Sym33 a = Sym33 { unSym33 :: [M33 a] } deriving (Eq, Hashable)
mkSym33 :: (Ord a, Hashable a) => M33 a -> Sym33 a
mkSym33 x = Sym33 $ sort $ hashNub $ f (mirM33 x) <> f x
    where f = take 4 . iterate rotM33

