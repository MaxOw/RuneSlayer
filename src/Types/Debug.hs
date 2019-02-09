{-# Language TemplateHaskell #-}
module Types.Debug where

import Delude

--------------------------------------------------------------------------------

data DebugFlag
   = DebugFlag_DrawPickupRange
   | DebugFlag_ZoomOutScroller
   | DebugFlag_HideScroller
   | DebugFlag_ShowDynamicBoundingBoxes
   deriving (Eq, Ord, Show)
