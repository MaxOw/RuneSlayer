module HasField where

import Control.Lens
-- import Types.Entity.Common

class HasLocation        s t | s -> t where location        :: Lens' s t
class HasVelocity        s t | s -> t where velocity        :: Lens' s t
class HasMaxSpeed        s t | s -> t where maxSpeed        :: Lens' s t
class HasEquipment       s t | s -> t where equipment       :: Lens' s t
class HasSlots           s t | s -> t where slots           :: Lens' s t
class HasItemType        s t | s -> t where itemType        :: Lens' s t
class HasItemKind        s t | s -> t where itemKind        :: Lens' s t
class HasContent         s t | s -> t where content         :: Lens' s t
class HasContainerType   s t | s -> t where containerType   :: Lens' s t
class HasName            s t | s -> t where name            :: Lens' s t
class HasVolume          s t | s -> t where volume          :: Lens' s t
class HasMaxVolume       s t | s -> t where maxVolume       :: Lens' s t
class HasEntityId        s t | s -> t where entityId        :: Lens' s t
class HasEntity          s t | s -> t where entity          :: Lens' s t
class HasSelfId          s t | s -> t where selfId          :: Lens' s t
class HasEntities        s t | s -> t where entities        :: Lens' s t
class HasOwner           s t | s -> t where owner           :: Lens' s t
class HasFittingSlots    s t | s -> t where fittingSlots    :: Lens' s t
class HasProcessOnUpdate s t | s -> t where processOnUpdate :: Lens' s t
class HasDebugFlags      s t | s -> t where debugFlags      :: Lens' s t
class HasRandomSeed      s t | s -> t where randomSeed      :: Lens' s t
class HasFrameCount      s t | s -> t where frameCount      :: Lens' s t
class HasLabel           s t | s -> t where label           :: Lens' s t
class HasContentVolume   s t | s -> t where contentVolume   :: Lens' s t

