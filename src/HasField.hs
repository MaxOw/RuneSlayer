{-# Language CPP #-}
{-# Language DefaultSignatures #-}
{-# Language MonoLocalBinds #-}
{-# Language OverloadedLabels #-}
{-# Language ScopedTypeVariables #-}
module HasField (module HasField) where

import Control.Lens
import Engine.HasField as HasField
import Data.Generics.Product as HasField (HasField'(field'))
import Data.Generics.Sum.Constructors
import GHC.TypeLits
import GHC.OverloadedLabels

--------------------------------------------------------------------------------

data SymbolProxy (s :: Symbol) = SProxy

instance (s0 ~ s1) => IsLabel (s0 :: Symbol) (SymbolProxy (s1 :: Symbol)) where
    fromLabel = SProxy

type HasF f = HasField' (AppendSymbol "field_" f)

ff :: forall f ff s a. (ff ~ AppendSymbol "field_" f, HasField' ff s a)
    => SymbolProxy f -> Lens' s a
ff SProxy = field' @ff

cc :: forall f ff s a. (ff ~ AppendSymbol "_" f, AsConstructor' f s a)
    => SymbolProxy ff -> Prism' s a
cc SProxy = _Ctor' @f

--------------------------------------------------------------------------------

class HasEntityId s a | s -> a where
    entityId :: Lens' s a
    default entityId :: HasF "entityId" s a => Lens' s a
    entityId = ff#entityId

class HasEntity s a | s -> a where
    entity :: Lens' s a
    default entity :: HasF "entity" s a => Lens' s a
    entity = ff#entity

class HasResources s a | s -> a where
    resources :: Lens' s a
    default resources :: HasF "resources" s a => Lens' s a
    resources = ff#resources

class HasAnimateWhenStopped s a | s -> a where
    animateWhenStopped :: Lens' s a
    default animateWhenStopped :: HasF "animateWhenStopped" s a => Lens' s a
    animateWhenStopped = ff#animateWhenStopped

class HasMaxSpeed s a | s -> a where
    maxSpeed :: Lens' s a
    default maxSpeed :: HasF "maxSpeed" s a => Lens' s a
    maxSpeed = ff#maxSpeed

--------------------------------------------------------------------------------

class GetZIndex          s t | s -> t where get_zindex      :: s -> t

type HasName            = HasF "name"
type HasKind            = HasF "kind"
type HasEra             = HasF "era"
type HasVelocity        = HasF "velocity"
type HasLocation        = HasF "location"
type HasDebugFlags      = HasF "debugFlags"
type HasProcessOnUpdate = HasF "processOnUpdate"
type HasAnimationState  = HasF "animationState"
type HasEffects         = HasF "effects"
type HasTimer           = HasF "timer"
type HasOwner           = HasF "owner"
type HasHealth          = HasF "health"
type HasEquipment       = HasF "equipment"
type HasCollisionShape  = HasF "collisionShape"
type HasUpdateOnce      = HasF "updateOnce"
type HasIsActive        = HasF "isActive"

#define MakeFieldLens(X) X :: HasF "X" s a => Lens' s a; X = ff#X

MakeFieldLens(name)
MakeFieldLens(self)
MakeFieldLens(target)
MakeFieldLens(bodyAnimation)
MakeFieldLens(itemAnimation)
MakeFieldLens(body)
MakeFieldLens(isMarked)
MakeFieldLens(unitType)
MakeFieldLens(animation)
MakeFieldLens(corpse)
MakeFieldLens(entityType)
MakeFieldLens(maxHealth)
MakeFieldLens(appearance)
MakeFieldLens(contentVolume)
MakeFieldLens(passiveType)
MakeFieldLens(containerType)
MakeFieldLens(maxVolume)
MakeFieldLens(volume)
MakeFieldLens(role)
MakeFieldLens(deleteSelf)
MakeFieldLens(actions)
MakeFieldLens(current)
MakeFieldLens(kind)
MakeFieldLens(frames)
MakeFieldLens(duration)
MakeFieldLens(title)
MakeFieldLens(resourceMap)
MakeFieldLens(spriteMap)
MakeFieldLens(animationsMap)
MakeFieldLens(tileSetMap)
MakeFieldLens(staticMap)
MakeFieldLens(itemsMap)
MakeFieldLens(unitsMap)
MakeFieldLens(pixelsPerUnit)
MakeFieldLens(tileSet)
MakeFieldLens(path)
MakeFieldLens(sprite)
MakeFieldLens(era)
MakeFieldLens(progression)
MakeFieldLens(speed)
MakeFieldLens(activatedList)
MakeFieldLens(dynamicIndex)
MakeFieldLens(entities)
MakeFieldLens(spatialIndex)
MakeFieldLens(lastId)
MakeFieldLens(unique)
MakeFieldLens(location)
MakeFieldLens(velocity)
MakeFieldLens(drawPickupRange)
MakeFieldLens(debugFlags)
MakeFieldLens(processOnUpdate)
MakeFieldLens(animationState)
MakeFieldLens(effects)
MakeFieldLens(effectUpdate)
MakeFieldLens(collisionShape)

MakeFieldLens(inputState)
MakeFieldLens(selectState)
MakeFieldLens(inventoryState)
MakeFieldLens(selectMap)
MakeFieldLens(selectKind)
MakeFieldLens(focusId)
MakeFieldLens(itemKind)
MakeFieldLens(equipment)
MakeFieldLens(label)
MakeFieldLens(prefix)
MakeFieldLens(currentPrefix)
MakeFieldLens(hint)
MakeFieldLens(hintMap)
MakeFieldLens(isFocused)
MakeFieldLens(focusedItem)
MakeFieldLens(owner)
MakeFieldLens(selfId)
MakeFieldLens(context)
MakeFieldLens(fittingSlots)
MakeFieldLens(frameCount)
MakeFieldLens(health)
MakeFieldLens(mode)
MakeFieldLens(active)
MakeFieldLens(deactivators)
MakeFieldLens(commonKeymap)
MakeFieldLens(inputKeymap)
MakeFieldLens(hist)
MakeFieldLens(values)
MakeFieldLens(visiblePanels)
MakeFieldLens(changeCache)
MakeFieldLens(menuState)
MakeFieldLens(gameScale)
MakeFieldLens(menuScale)
MakeFieldLens(scroller)
MakeFieldLens(reactivity)
MakeFieldLens(timer)
MakeFieldLens(config)
MakeFieldLens(debugMode)
MakeFieldLens(status)
MakeFieldLens(runeSet)
MakeFieldLens(weaponKind)
MakeFieldLens(isActive)
