{-# Language CPP #-}
{-# Language DefaultSignatures #-}
{-# Language MonoLocalBinds #-}
module HasField (module HasField) where

import Control.Lens
-- import Types.Entity.Common
import Engine.HasField as HasField
import Data.Generics.Product as HasField (HasField'(field'))

class HasEntityId s a | s -> a where
    entityId :: Lens' s a
    default entityId :: HasField' "field_entityId" s a => Lens' s a
    entityId = field' @"field_entityId"

class HasEntity s a | s -> a where
    entity :: Lens' s a
    default entity :: HasField' "field_entity" s a => Lens' s a
    entity = field' @"field_entity"

class HasResources s a | s -> a where
    resources :: Lens' s a
    default resources :: HasField' "field_resources" s a => Lens' s a
    resources = field' @"field_resources"

class GetZIndex          s t | s -> t where get_zindex      :: s -> t

type HasName            = HasField' "field_name"
type HasKind            = HasField' "field_kind"
type HasEra             = HasField' "field_era"
type HasVelocity        = HasField' "field_velocity"
type HasMaxSpeed        = HasField' "field_maxSpeed"
type HasLocation        = HasField' "field_location"
type HasDebugFlags      = HasField' "field_debugFlags"
type HasProcessOnUpdate = HasField' "field_processOnUpdate"
type HasAnimationState  = HasField' "field_animationState"
type HasEffects         = HasField' "field_effects"
type HasOwner           = HasField' "field_owner"
type HasHealth          = HasField' "field_health"
type HasEquipment       = HasField' "field_equipment"
type HasCollisionShape  = HasField' "field_collisionShape"

#define MakeField2(X,FX) X :: HasField' "FX" s a => Lens' s a; X = field' @"FX"
#define MakeField(X) MakeField2(X,field_/**/X)

MakeField(name)
MakeField(self)
MakeField(target)
MakeField(bodyAnimation)
MakeField(body)
MakeField(isMarked)
MakeField(unitType)
MakeField(animation)
MakeField(corpse)
MakeField(entityType)
MakeField(maxHealth)
MakeField(appearance)
MakeField(contentVolume)
MakeField(itemType)
MakeField(containerType)
MakeField(maxVolume)
MakeField(volume)
MakeField(role)
MakeField(deleteSelf)
MakeField(actions)
MakeField(current)
MakeField(kind)
MakeField(frames)
MakeField(duration)
MakeField(desc)
MakeField(title)
MakeField(resourceMap)
MakeField(spriteMap)
MakeField(animationsMap)
MakeField(tileSetMap)
MakeField(staticMap)
MakeField(itemsMap)
MakeField(unitsMap)
MakeField(pixelsPerUnit)
MakeField(tileSet)
MakeField(path)
MakeField(sprite)
MakeField(era)
MakeField(progression)
MakeField(speed)
MakeField(activatedList)
MakeField(dynamicIndex)
MakeField(action)
MakeField(entities)
MakeField(spatialIndex)
MakeField(lastId)
MakeField(unique)
MakeField(location)
MakeField(velocity)
MakeField(maxSpeed)
MakeField(drawPickupRange)
MakeField(debugFlags)
MakeField(processOnUpdate)
MakeField(animationState)
MakeField(effects)
MakeField(effectUpdate)
MakeField(collisionShape)

MakeField(gameState)
MakeField(inputState)
MakeField(selectState)
MakeField(inventoryState)
MakeField(selectMap)
MakeField(selectKind)
MakeField(focusId)
MakeField(itemKind)
MakeField(equipment)
MakeField(label)
MakeField(prefix)
MakeField(currentPrefix)
MakeField(hint)
MakeField(hintMap)
MakeField(isFocused)
MakeField(focusedItem)
MakeField(owner)
MakeField(selfId)
MakeField(context)
MakeField(fittingSlots)
MakeField(frameCount)
MakeField(health)
MakeField(mode)
MakeField(active)
MakeField(deactivators)
MakeField(commonKeymap)
MakeField(inputKeymap)
MakeField(hist)
MakeField(values)
MakeField(visiblePanels)
MakeField(changeCache)
MakeField(menuState)
MakeField(gameScale)
MakeField(menuScale)
MakeField(scroller)
