module Types.Tutorial where

import Delude
import Types.InputAction (InputAction)
import Types.Entity.Common (Duration)
import Data.Timer

data TutorialStep
   = TutorialStep_Start
   | TutorialStep_Movement
   | TutorialStep_PickingUpItems
   | TutorialStep_Inventory
   | TutorialStep_Interaction
   | TutorialStep_Runes
   | TutorialStep_Attack
   | TutorialStep_Done
   deriving (Eq, Ord, Enum, Bounded, Generic)
instance Default TutorialStep where def = TutorialStep_Start

data TimerType
   = TimerType_MinPageTime
   | TimerType_MaxPageTime
   | TimerType_PostPageDelay
   deriving (Eq, Ord, Generic)

data TutorialPage = TutorialPage
   { field_title   :: Text
   , field_content :: Text
   , field_timer   :: Maybe Duration
   } deriving (Generic)
instance Default TutorialPage

data TutorialState = TutorialState
   { field_activatedInputActions :: Set InputAction
   , field_satisfied             :: Set TutorialStep
   , field_currentStep           :: TutorialStep
   , field_currentPage           :: Maybe TutorialPage
   , field_timer                 :: Timer TimerType
   } deriving (Generic)
instance Default TutorialState

activatedInputActions :: Lens' TutorialState (Set InputAction)
activatedInputActions = ff#activatedInputActions

satisfied :: Lens' TutorialState (Set TutorialStep)
satisfied = ff#satisfied

currentStep :: Lens' TutorialState TutorialStep
currentStep = ff#currentStep

currentPage :: Lens' TutorialState (Maybe TutorialPage)
currentPage = ff#currentPage
