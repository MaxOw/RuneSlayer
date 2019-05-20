module Entity.Effect
    ( Effect
    , effectToEntity, makeEffect
    )where

import Delude

import Entity.Actions (locate, withZIndex)
import Entity.Utils
import Types.Entity.Effect

import qualified Diagrams.TwoD.Transform as T
import qualified Color

actOn :: Effect -> EntityAction -> Effect
actOn x _ = x

update :: Effect -> EntityContext -> Q (Maybe Effect, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    if | x^.era >= x^.duration._Wrapped -> deleteSelf .= True
       | otherwise                      -> self.era   += defaultDelta^._Wrapped

render :: Effect -> RenderContext -> RenderAction
render x _ctx = withZIndex x $ locate x $
    translateY 0.8 $ renderEffect (x^.kind)
    where
    renderEffect = \case
        HitEffect a -> translateY (x^.era) $ renderHit a
        where
        renderHit (AttackPower a) = T.scale (1/64) $ renderSimpleText d $ show (-a)
        d = def & color .~ Color.opaque Color.red

oracle :: Effect -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Location -> Just $ x^.location
    _                    -> Nothing

--------------------------------------------------------------------------------

effectToEntity :: Effect -> Entity
effectToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = const Nothing
   , makeKind   = EntityKind_Dynamic
   }

makeEffect :: EffectKind -> Effect
makeEffect x = def & kind .~ x

