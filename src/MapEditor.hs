module MapEditor
    ( init
    , handleActivation
    , renderSelected
    , saveAdHoc
    ) where

import Delude hiding (init)
import Engine (EngineState, RenderAction, userState)
import Types (Game, St)
import Types.GameState (gameState)
import Types.MapEditor
import Types.DirectedAction
import Types.EntityAction
import Types.ResourceManager
import Types.Entity
import Focus (cameraLocation)
import GameState.Actions
import qualified Data.Zipper as Zipper
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import qualified Data.Text as Text

data FChange f = FChange { runFChange :: forall a. f a -> f a }

--------------------------------------------------------------------------------

init :: Resources -> MapEditorState
init rs = execState ?? def $ do
    passives .= Zipper.fromList (HashMap.keys $ rs^.passiveMap)
    agents   .= Zipper.fromList (HashMap.keys $ rs^.agentsMap)

handleActivation :: MapEditorAction -> Game ()
handleActivation = \case
    MapEditorAction_PlaceEntity  -> placeEntity
    MapEditorAction_NextCategory -> changeCategory next
    MapEditorAction_PrevCategory -> changeCategory prec
    MapEditorAction_SelectNext   -> changeSelect $ FChange Zipper.rightCycle
    MapEditorAction_SelectPrev   -> changeSelect $ FChange Zipper.leftCycle
    where
    placeEntity = whenJustM cameraLocation $ \cloc -> do
        whenJustM (use selectedEntity) $ \s -> do
            let se = s^.ff#spawnEntity
            mapEditorState.placedList %= ((se, cloc):)
            actOnWorld $ WorldAction_SpawnEntity se $ def & set actions
                [ EntityAction_SetValue $ EntityValue_Location cloc ]

    changeCategory f = do
        mapEditorState.selectorCategory %= f
        changeSelect $ FChange id

    changeSelect f = do
        me <- changeSelectFor f =<< use (mapEditorState.selectorCategory)
        rs <- use $ userState.resources
        let cx = RenderContext
               { field_resources = rs
               , field_debugFlags = mempty }
        let en = fromSpawnEntity rs =<< me
        assign selectedEntity $ SelectedEntity
            <$> me
            <*> fmap (flip entityRender cx) en

    changeSelectFor f = \case
        SelectorCategory_Passives -> do
            mapEditorState.passives %= runFChange f
            mf <- uses (mapEditorState.passives) Zipper.focus
            return $ fmap SpawnEntity_Passive mf
        SelectorCategory_Agents -> do
            mapEditorState.agents   %= runFChange f
            mf <- uses (mapEditorState.agents) Zipper.focus
            return $ fmap SpawnEntity_Agent mf

renderSelected :: Game RenderAction
renderSelected = use (selectedEntity.traverse.renderAction)

saveAdHoc :: Game ()
saveAdHoc = whenNotNullM (use $ mapEditorState.placedList) $ \ls -> do
    let ps = mapMaybe getPassives $ toList ls
    let as = mapMaybe getAgents   $ toList ls
    let out = [ "-- Passives"] <> ps <> ["","-- Agents"] <> as
    writeFileText "adhoc-map-editor.ignore" $ Text.unlines out
    where
    getPassives (SpawnEntity_Passive x, l) = Just $ writePassive x l
    getPassives _ = Nothing

    getAgents (SpawnEntity_Agent x, l) = Just $ writeAgent x l
    getAgents _ = Nothing

    writePassive n l =
        fromString $ printf "  , placeAt %.1f %.1f item.%s" x y nn
        where
        V2 x y = Unwrapped l
        nn = Unwrapped n

    writeAgent n l =
        fromString $ printf "  , spawnAt %.1f %.1f agent.%s" x y nn
        where
        V2 x y = Unwrapped l
        nn = Unwrapped n

--------------------------------------------------------------------------------

mapEditorState :: Lens' (EngineState St) MapEditorState
mapEditorState = gameState.ff#mapEditorState

selectedEntity :: Lens' (EngineState St) (Maybe SelectedEntity)
selectedEntity = mapEditorState.ff#selectedEntity

