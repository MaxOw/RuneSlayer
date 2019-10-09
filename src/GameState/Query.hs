module GameState.Query where

import Delude
import qualified Data.Set  as Set

import Types (Game)
import Types.GameState (gameState)
import Types.Entity (Entity)
import Types.Entity.Common (Volume)

import EntityIndex
import Entity

--------------------------------------------------------------------------------

canFitIntoContainer
    :: HasEntity i Entity
    => HasEntity c Entity
    => i -> c -> Game Bool
canFitIntoContainer (view entity -> item) (view entity -> container) = do
    maybeFreeVolume <- calcFreeVolume container
    return $ doMaybe maybeFreeVolume
    where
    doMaybe maybeFreeVolume = fromMaybe False $ do
        fv  <- maybeFreeVolume
        iv  <- item^.oracleVolume
        ipt <- item^.oraclePassiveType
        cpt <- container^.oraclePassiveType
        ct  <- cpt^.containerType
        let pk = ipt^.ff#passiveKind
        let ak = ct^.ff#allowKinds
        return $ iv <= fv && (not $ Set.disjoint pk ak)

calcFreeVolume
    :: HasEntity e Entity
    => e -> Game (Maybe Volume)
calcFreeVolume (view entity -> e) = case e^.oracleMaxVolume of
    Nothing -> return Nothing
    Just mv -> do
        let es = e^.oracleContent.traverse
        vs <- sum . mapMaybe (view (entity.oracleVolume)) <$> lookupEntities es
        return $ Just $ max 0 $ mv - vs

--------------------------------------------------------------------------------

lookupEntities :: Foldable t => t EntityId -> Game [EntityWithId]
lookupEntities is = do
    es <- use $ gameState.entities
    EntityIndex.lookupManyById is es

lookupEntity :: EntityId -> Game (Maybe EntityWithId)
lookupEntity i = do
    es <- use $ gameState.entities
    EntityIndex.lookupById i es

