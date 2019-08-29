module Game
    ( initSt, endSt
    ) where

import Delude hiding (context)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine.Graphics.Utils (delObject)
import Graphics.GL (glDeleteTextures)
import Engine (RenderAction, Img, userState, texture, context)
import Engine (FontName, fontBase, fontBold, fontBoldItalic, fontItalic)
import qualified Diagrams.TwoD.Transform as T
import Engine.Types (Engine, graphics)
import Engine.Graphics.Scroller (newScroller)
import Engine.Graphics.Scroller.Types (Scroller)
import Types.GameState
import Types.InputState (defaultInputState)
import Types.Entity (EntityIndex)
import Types.Config
import Types.St
import Types.Entity.Common
import Entity.Agent (makeAgent)
import Types.ResourceManager
import Types.DirectedAction
import EntityLike
import WorldGen
import Types.EntityAction (Spawn, EntityAction)
import Types.Entity.Agent (AgentTypeName)
import Types.Entity.Passive (PassiveTypeName)
import Types.Entity.Script (StoryDialogName, StoryDialog)
import Skills.Runes (RuneSet, buildRuneSet)
import Types.Entity.Animation
import qualified Entity.Animation as Animation

-- import qualified Resource
import qualified EntityIndex
import EntityIndex (EntityIndexTag(..))
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.Collider as Collider

import Dhall.Utils (dhallToMap, loadDhall, inputAuto)
import qualified Tutorial

descPath :: FilePath
descPath = "data/desc"

initSt :: Engine () St
initSt = do
    conf   <- loadDhall "" "Config.dhall"
    wgconf <- inputAuto descPath "./WorldGen.dhall"

    scro <- newScroller $ def
        & bufferMargin .~ Engine.ScrollerMargin_Pixels (64 + 8)
    eix <- EntityIndex.new $ def & size .~ (wgconf^.size)
    st <- defaultSt eix scro

    loadFonts
    rs <- loadResources conf
    Engine.fullyUpdateAtlas
    Engine.setDefaultFonts ["Arial"] 10

    let world = generateWorld rs wgconf
    rnd <- makeRenderOverview world
    whenNothing_ (conf^.debugMode) $
        forM_ (world^.entities) $ \e -> EntityIndex.insert e eix

    let spawnUnits = map spawnAgentToAction   $ wgconf^.ff#units
    let spawnItems = map spawnPassiveToAction $ wgconf^.ff#items

    pli <- loadDhall descPath "Player.dhall"
    pid <- EntityIndex.insert (playerEntity rs pli) eix
    EntityIndex.addTag EntityIndexTag_Camera pid eix
    EntityIndex.addTag EntityIndexTag_Player pid eix
    liftIO . GLFW.showWindow =<< use (graphics.context)
    return $ st
        & gameState.focusId .~ Just pid
        & gameState.actions .~ spawnUnits <> spawnItems
        & resources  .~ rs
        & overview   .~ rnd
        & config     .~ conf
    where
    playerEntity rs pli = toEntity $ makeAgent rs pli
        & location .~ locM 0 0
        & collisionShape .~ Just (Collider.circle 0 0.3)

    overview = ff#overview

loadRuneSet :: Config -> Engine us RuneSet
loadRuneSet conf = do
    let rsname = conf^.ff#runeSet
    rus <- loadDhall descPath "RuneSets.dhall"
    case Map.lookup rsname rus of
        Nothing -> error $ "Unable to load RuneSet: " <> rsname
        Just rp -> buildRuneSet <$> loadDhall descPath rp

makeRenderOverview :: WorldGenOutput -> Engine us RenderAction
makeRenderOverview out = case out^.overviewImage of
    Nothing -> return mempty
    Just oi -> do
        img <- Engine.addImageToAtlas oi
        return $ T.scale 4 $ Engine.renderImg img

loadResources :: Config -> Engine us Resources
loadResources conf = case conf^.debugMode of
    Just _m -> do
        ts <- loadDhallList "TileSets.dhall"
        ps <- loadDhallList "PassiveTypes.dhall"
        return $ def
            & tileSetMap .~ buildMap ts
            & passiveMap .~ buildMap ps
    Nothing -> do
        rs <- loadAllPaths
        ss <- loadDhallMap  "Sprites.dhall"
        ts <- loadDhallList "TileSets.dhall"
        ps <- loadDhallList "PassiveTypes.dhall"
        us <- loadDhallList "UnitTypes.dhall"
        as <- loadDhallMap  "Animations.dhall"

        dialogs <- loadDialogs
        -- mapM_ print dialogs

        rust <- loadRuneSet conf

        let res = def
                & imgMap        .~ HashMap.fromList rs
                & spriteMap     .~ ss
                & tileSetMap    .~ buildMap ts
                & passiveMap    .~ buildMap ps
                & agentsMap     .~ buildMap us
                & dialogMap     .~ dialogs
                & runeSet       .~ rust
        let pr = fmap (Animation.makeAnimation res)
               $ HashMap.fromList
               $ map (over _1 AnimationName)
               $ HashMap.toList as
        return $ res
            & animationsMap .~ pr

loadDialogs :: Engine us (HashMap StoryDialogName StoryDialog)
loadDialogs = do
    HashMap.fromList <$> inputAuto descPath "./Script/StoryDialog.dhall"

buildMap :: (HasName x name, Eq name, Hashable name) => [x] -> HashMap name x
buildMap = HashMap.fromList . map (\x -> (x^.name, x))

loadAllPaths :: Engine us [(Text, Img)]
loadAllPaths = do
    hm <- liftIO $ dhallToMap descPath "ResourcePaths.dhall"
    catMaybes <$> mapM loadResource (ordNub $ HashMap.elems hm)

loadDhallList :: FromJSON a => FilePath -> Engine us [a]
loadDhallList = fmap HashMap.elems . liftIO . dhallToMap descPath

loadDhallMap :: FromJSON a => FilePath -> Engine us (HashMap Text a)
loadDhallMap = liftIO . dhallToMap descPath

endSt :: Engine St ()
endSt = do
    rs <- uses (userState.resources.imgMap) HashMap.elems
    mapM_ (delObject glDeleteTextures . view texture) rs

loadResource :: Text -> Engine us (Maybe (Text, Img))
loadResource r = do
    -- putStr $ "Loading resource: " <> r -- Resource.resource_path r
    mi <- Engine.loadTextureToAtlas $ toString $ r -- Resource.resource_path r
    -- putStrLn $ if isJust mi then " [Success]" else " [Failure]" :: Text
    return $ (r,) <$> mi

loadFonts :: Engine us ()
loadFonts = do
    loadFontFamily "Arial" ".ttf"
    loadFontBase "SourceHanSerif" "-Regular.otf"

loadFontBase :: FontName -> Text -> Engine us ()
loadFontBase fname ext = void $ Engine.loadFontFamily fname $ def
    & fontBase .~ mkFontPath fname ""
    where
    mkFontPath n s = toString $ "data/fonts/" <> n <> s <> ext

loadFontFamily :: FontName -> Text -> Engine us ()
loadFontFamily fname ext = void $ Engine.loadFontFamily fname $ def
    & fontBase              .~ mkFontPath fname ""
    & fontBold              .~ Just (mkFontPath fname "-Bold")
    & fontBoldItalic        .~ Just (mkFontPath fname "-Bold-Italic")
    & fontItalic            .~ Just (mkFontPath fname "-Italic")
    where
    mkFontPath n s = toString $ "data/fonts/" <> n <> s <> ext

spawnAgentToAction :: Spawn AgentTypeName EntityAction -> DirectedAction
spawnAgentToAction a = directAtWorld
    $ WorldAction_SpawnEntity (SpawnEntity_Agent $ a^.name) $ def
        & actions .~ a^.actions

spawnPassiveToAction :: Spawn PassiveTypeName EntityAction -> DirectedAction
spawnPassiveToAction a = directAtWorld
    $ WorldAction_SpawnEntity (SpawnEntity_Passive $ a^.name) $ def
        & actions .~ a^.actions

--------------------------------------------------------------------------------

defaultSt :: MonadIO m => EntityIndex -> Scroller -> m St
defaultSt eix scro = do
    gs <- defaultGameState eix
    return $ St
        { field_resources  = def
        , field_inputState = defaultInputState
        , field_gameState  = gs
        , field_menuState  = def
        , field_scroller   = scro
        , field_debugFlags = def
        , field_overview   = mempty
        , field_config     = def
        , field_wires      = def
        }

defaultGameState :: MonadIO m => EntityIndex -> m GameState
defaultGameState eix = do
    return $ GameState
        { field_entities       = eix
        , field_actions        = []
        , field_focusId        = Nothing
        , field_gameScale      = 64
        , field_menuScale      = 1.0
        , field_frameCount     = 0
        , field_changeCache    = mempty
        , field_gameOverScreen = Nothing
        , field_tutorialState  = Tutorial.defaultTutorialState
        , field_systemMessages = def
        }

