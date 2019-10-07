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
actOn x a = x & case a of
    EntityAction_SetValue v -> handleSetValue v
    _ -> id
    where
    handleSetValue ev = case ev of
        EntityValue_Location  v -> set location v
        _                       -> id


update :: Effect -> EntityContext -> Q (Maybe Effect, [DirectedAction])
update x ctx = runUpdate x ctx $ do
    if | x^.era >= x^.duration._Wrapped -> deleteSelf .= True
       | otherwise                      -> self.era   += defaultDelta^._Wrapped

render :: Effect -> RenderContext -> RenderAction
render x _ctx = withZIndex x $ locate x $ correctHeight $ renderEffect (x^.kind)
    where
    renderEffect = \case
        HitEffect a -> translateY (x^.era) $ renderHit a
        where
        renderHit a = mempty
        -- renderHit a = T.scale (1/64) $ renderSimpleText d $ show (-a^._Wrapped)
        -- d = def & color .~ Color.opaque Color.red

oracle :: Effect -> EntityQuery a -> Maybe a
oracle x = \case
    EntityQuery_Name     -> Just $ kindToName $ x^.kind
    EntityQuery_Location -> Just $ x^.location
    _                    -> Nothing
    where
    kindToName :: EffectKind -> Text
    kindToName = \case
        HitEffect {} -> "HitEffect"

--------------------------------------------------------------------------------

effectToEntity :: Effect -> Entity
effectToEntity = makeEntity $ EntityParts
   { makeActOn  = actOn
   , makeUpdate = update
   , makeRender = render
   , makeOracle = oracle
   , makeSave   = const Nothing
   , makeKind   = const EntityKind_Dynamic
   }

makeEffect :: EffectKind -> Effect
makeEffect x = def & kind .~ x

