module InputKeymap.Default (defaultBindings) where

import Delude
import qualified Types.Entity.Animation as Animation
import Types.InputAction
import Types.InputKeymap
import Types.EntityAction (AttackMode(..))
import Types.MapEditor
import Types.Debug (DebugFlag(..))

defaultBindings :: [KeymapEntry Text]
defaultBindings =
    [ mapAll "<Escape>" InputAction_Escape
    , mapAll "Q" FastQuit

    , nmap "i" (SetMode InventoryMode)

    , nmap "j" (SimpleMove MoveDown)
    , nmap "k" (SimpleMove MoveUp)
    , nmap "h" (SimpleMove MoveLeft)
    , nmap "l" (SimpleMove MoveRight)

    -- , nmap "tg" (ToggleViewPanel GroundPreviewPanel)
    , nmap "Dtr" (ToggleDebug DebugFlag_DrawPickupRange)
    , nmap "Dts" (ToggleDebug DebugFlag_ZoomOutScroller)
    , nmap "Dtb" (ToggleDebug DebugFlag_HideScroller)
    , nmap "Dtd" (ToggleDebug DebugFlag_ShowDynamicBoundingBoxes)
    , nmap "Dtc" (ToggleDebug DebugFlag_ShowCollisionShapes)

    , nmap "Dac" (DebugRunAnimation Animation.Cast)
    , nmap "Dat" (DebugRunAnimation Animation.Thrust)
    , nmap "Daw" (DebugRunAnimation Animation.Walk)
    , nmap "Das" (DebugRunAnimation Animation.Slash)
    , nmap "Daf" (DebugRunAnimation Animation.Fire)
    , nmap "Dad" (DebugRunAnimation Animation.Die)

    , nmap "yy" PickupAllItems
    -- , nmap "yi" SelectItemToPickUp
    , nmap "pp" DropAllItems

    , nmap "g" ExecuteAttack

    , nmap "ss" SwapWeapon

    , nmap "mm" (SetAttackMode AttackMode_Manual)
    , nmap "ma" (SetAttackMode AttackMode_Auto)

    , nmap "M" (SetMode MapEditorMode)

    , nmap "rr" StartRunicMode
    , nmap "rs" (SetMode RunicStatusMode)

    , nmap "f" Interact
    , nmap "F" SelectInteraction

    , nmap "Ts" (TutorialAction TutorialAction_SkipAll)
    , nmap "Tr" (TutorialAction TutorialAction_Restart)


    , mapMode InventoryMode "yy" PickupAllItems
    , mapMode InventoryMode "yi" SelectItemToPickUp

    , mapMode InventoryMode "pp" DropAllItems
    , mapMode InventoryMode "pi" SelectItemToDrop

    , mapMode InventoryMode "m" SelectItemToMove
    , mapMode InventoryMode "M" SelectItemMoveTarget

    , mapMode InventoryMode "f" SelectItemToFocus
    , mapMode InventoryMode "u" UseFocusedItem

    , mapMode InventoryMode "ss" SwapWeapon


    , unmapMode StoryDialogMode "<Escape>"
    , mapMode StoryDialogMode "<Space>"  InputAction_NextPage


    , unmapMode StoryMode "<Escape>"


    , mapMode MapEditorMode "j" (SimpleMove MoveDown)
    , mapMode MapEditorMode "k" (SimpleMove MoveUp)
    , mapMode MapEditorMode "h" (SimpleMove MoveLeft)
    , mapMode MapEditorMode "l" (SimpleMove MoveRight)

    , mapMode MapEditorMode "n" (MapEditorAction MapEditorAction_SelectNext)
    , mapMode MapEditorMode "p" (MapEditorAction MapEditorAction_SelectPrev)
    , mapMode MapEditorMode "m" (MapEditorAction MapEditorAction_NextCategory)
    , mapMode MapEditorMode "<Space>" (MapEditorAction MapEditorAction_PlaceEntity)
    ]
    where
    nmap          = mapMode NormalMode
    mapAll k a    = KeymapEntry Nothing k (Just a)
    mapMode m k a = KeymapEntry (Just m) k (Just a)
    unmapMode m k = KeymapEntry (Just m) k Nothing

