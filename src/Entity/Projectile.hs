module Entity.Projectile
    ( Projectile(..), projectileToEntity
    ) where

import Delude

import Entity
import Entity.Utils
import Entity.Actions
import Types.Entity.Projectile
import Diagrams.Angle (_theta)
import qualified Diagrams.Angle     as T
import qualified Diagrams.Direction as T

--------------------------------------------------------------------------------

actOn :: Projectile -> EntityAction -> Projectile
actOn x _a = x

update :: Projectile -> EntityContext -> Q (Maybe Projectile, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    oldLoc <- use $ self.location
    integrateLocation
    newLoc <- use $ self.location
    dl <- self.distanceLeft <%= (\a -> a - distanceBetween oldLoc newLoc)
    when (dl<=0) $ deleteSelf .= True

    whenJustM (queryById =<< use (self.target)) $ \t -> do
        effectiveDistance <- getEffectiveDistance
        dtt <- fromMaybe effectiveDistance <$> distanceToEntity t
        when (dtt < effectiveDistance) (doDemage t)

doDemage :: EntityWithId -> Update Projectile ()
doDemage eid = do
    ap <- use $ self.attackPower
    pt <- use $ self.passiveType.ff#stats.ff#attack
    msource <- use $ self.ff#source
    addAction eid $ EntityAction_SelfAttacked (ap + pt) msource
    deleteSelf .= True

getEffectiveDistance :: Update Projectile Distance
getEffectiveDistance = return $ Distance 1

render :: Projectile -> RenderContext -> RenderAction
render x ctx = withZIndex x $ locate x $ correctHeight
    $ T.rotate velocityAngle
    $ renderAppearance ctx $ x^.passiveType.appearance
    where
    velocityAngle = view _theta $ T.direction (x^.velocity._Wrapped)
    correctHeight = translateY 0.8 -- TODO: this souldn't be hardcoded

oracle :: Projectile -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location      -> Just $ x^.location
    EntityQuery_Name          -> Just $ x^.passiveType.name._Wrapped
    _                         -> Nothing

--------------------------------------------------------------------------------

projectileToEntity :: Projectile -> Entity
projectileToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = const Nothing -- Just . EntitySum_Projectile
   , makeKind   = const EntityKind_Dynamic
   }

--------------------------------------------------------------------------------

distanceLeft :: Lens' Projectile Distance
distanceLeft = ff#distanceLeft

attackPower :: Lens' Projectile AttackPower
attackPower = ff#attackPower
