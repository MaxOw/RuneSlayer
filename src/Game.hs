{-# Language BangPatterns #-}
module Game
    ( initSt, setupSt, endSt
    ) where

import Delude hiding (context)
import System.FilePath ((</>))
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine.Graphics.Utils (delObject)
import Graphics.GL (glDeleteTextures)
import Engine (Img, userState, texture, context, getTime)
import Engine (fontBase, fontBold, fontBoldItalic, fontItalic)
import Engine.Types (Engine, graphics)
import Types.GameState
import Types.InputState (defaultInputState)
import Types.Entity.Common (EntityKind(..))
import Types.Entity (EntityIndex, Entity)
import Types.Config
import Types.St
import Entity.Agent (makeAgent)
import Types.ResourceManager
import Types.DirectedAction
import EntityLike
import Entity
import WorldGen
import Types.EntityAction (Spawn, EntityAction)
import Types.Entity.Agent (AgentTypeName)
import Types.Entity.Passive (PassiveTypeName)
import Skills.Runes (RuneSet, buildRuneSet)
import Types.Entity.Animation
import qualified Entity.Animation as Animation

import InputKeymap (buildInputKeymap, defaultBindings)
import GameState.Query (isConfigDebugFlagOn)
import qualified EntityIndex
import EntityIndex (EntityIndexTag(..))
import qualified Graphics.UI.GLFW as GLFW

import qualified Engine.Graphics.Scroller.Cells as Scroller

import Dhall.Utils (dhallToMap, loadDhall, inputAuto)
import qualified Tutorial
import qualified Runes
import qualified MapEditor
import qualified Story
-- import qualified Schema

import qualified Data.BitSet as BitSet

-- import Types.Schema.WorldMap ()
-- import Criterion.Measurement (secs)

printTimer :: MonadIO m => Float -> String -> m ()
printTimer _st _msg = do
    _ed <- getTime
    -- putStrLn $ msg <> ": " <> secs (realToFrac $ ed - st)
    return ()

initSt :: Engine () St
initSt = do
    timer0 <- getTime
    conf <- inputAuto @Config "" "./Config.dhall"
    let dhallPath = getDhallPath conf
    printTimer timer0 "Timer0A"
    wgconf <- inputAuto @WorldGenConfig dhallPath "./WorldGen.dhall"
    printTimer timer0 "Timer0"

    timer1 <- getTime
    eix <- EntityIndex.new $ def & size .~ (wgconf^.size)
    st <- initBaseSt conf eix
    printTimer timer1 "Timer1"

    timer2 <- getTime
    loadFonts conf
    rs <- loadResources conf
    Engine.fullyUpdateAtlas
    printTimer timer2 "Timer2"

    let mes = MapEditor.init rs

    timerWG <- getTime
    let world = generateWorld rs wgconf
    -- rnd <- makeRenderOverview world
    forM_ (world^.entities) $ \e -> EntityIndex.insert e eix
    forM_ (world^.entities) $ addStaticShape eix
    printTimer timerWG "TimerWG"

    let spawnUnits = map spawnAgentToAction   $ wgconf^.ff#units
    let spawnItems = map spawnPassiveToAction $ wgconf^.ff#items

    timer3 <- getTime
    pli <- loadDhall dhallPath "Player.dhall"
    let ploc = wgconf^.ff#startLocation
    pid <- EntityIndex.insert (playerEntity rs pli ploc) eix
    EntityIndex.addTag EntityIndexTag_Camera pid eix
    EntityIndex.addTag EntityIndexTag_Focus  pid eix
    printTimer timer3 "Timer3"
    printTimer timer0 "Timer All"
    liftIO . GLFW.showWindow =<< use (graphics.context)
    return $ execState ?? st $ do
        gameState.actions        .= spawnUnits <> spawnItems
        gameState.mapEditorState .= mes
        resources                .= rs
     -- overview                 .= rnd
        config                   .= conf
    where
    playerEntity rs pli ploc = toEntity $ makeAgent rs pli
        & location .~ ploc

    -- overview = ff#overview
    mapEditorState = ff#mapEditorState

addStaticShape :: EntityIndex -> Entity -> Engine us ()
addStaticShape eix e = when (entityKind e == EntityKind_Tile) $ do
    let mp = e^?oracleLocation.traverse._Wrapped
    let ms = e^.oracleCollisionShape
    let mb = e^.oracleCollisionBits
    whenJust ((,,) <$> mp <*> ms <*> mb) $ \(p, s, b) -> do
        let v = StaticShape p s b
        when (not $ BitSet.null b) $ EntityIndex.insertStaticShape v eix

setupSt :: Game ()
setupSt = do
    whenFlag ConfigDebugFlag_NoTutorial Tutorial.skipAll
    whenFlag ConfigDebugFlag_NoStory    Story.noStory
    where
    whenFlag = whenM . isConfigDebugFlagOn

loadRuneSet :: MonadIO m => Config -> m RuneSet
loadRuneSet conf = do
    let rsname = conf^.ff#runeSet
    let dhallPath = getDhallPath conf
    rus <- loadDhall dhallPath "RuneSets.dhall"
    case Map.lookup rsname rus of
        Nothing -> error $ "Unable to load RuneSet: " <> rsname
        Just rp -> buildRuneSet <$> loadDhall dhallPath rp

{-
makeRenderOverview :: WorldGenOutput -> Engine us RenderAction
makeRenderOverview out = case out^.overviewImage of
    Nothing -> return mempty
    Just oi -> do
        img <- Engine.addImageToAtlas oi
        return $ T.scale 4 $ Engine.renderImg img
-}

getDhallPath :: Config -> FilePath
getDhallPath = toString . fromMaybe "dhall" . view (ff#dhallPath)

loadResources :: Config -> Engine us Resources
loadResources conf = do
    let dhallPath = getDhallPath conf
    rs <- loadAllPaths  dhallPath
    ss <- loadDhallMap  dhallPath "Sprites.dhall"
    ts <- loadDhallList dhallPath "TileSets.dhall"
    ps <- loadDhallList dhallPath "PassiveTypes.dhall"
    us <- loadDhallList dhallPath "UnitTypes.dhall"
    as <- loadDhallMap  dhallPath "Animations.dhall"

    let res = def
            & imgMap        .~ HashMap.fromList rs
            & spriteMap     .~ ss
            & tileSetMap    .~ buildMap ts
            & passiveMap    .~ buildMap ps
            & agentsMap     .~ buildMap us
    let pr = fmap (Animation.makeAnimation res)
            $ HashMap.fromList
            $ map (over _1 AnimationName)
            $ HashMap.toList as
    return $ res
        & animationsMap .~ pr

buildMap :: (HasName x name, Eq name, Hashable name) => [x] -> HashMap name x
buildMap = HashMap.fromList . map (\x -> (x^.name, x))

loadAllPaths :: FilePath -> Engine us [(Text, Img)]
loadAllPaths dhallPath = do
    hm <- liftIO $ dhallToMap dhallPath "ResourcePaths.dhall"
    catMaybes <$> mapM loadResource (ordNub $ HashMap.elems hm)

loadDhallList :: FromJSON a => FilePath -> FilePath -> Engine us [a]
loadDhallList dp = fmap HashMap.elems . liftIO . dhallToMap dp

loadDhallMap :: FromJSON a => FilePath -> FilePath -> Engine us (HashMap Text a)
loadDhallMap dp = liftIO . dhallToMap dp

endSt :: Engine St ()
endSt = do
    MapEditor.saveAdHoc
    rs <- uses (userState.resources.imgMap) HashMap.elems
    mapM_ (delObject glDeleteTextures . view texture) rs

loadResource :: Text -> Engine us (Maybe (Text, Img))
loadResource r = do
    -- putStr $ "Loading resource: " <> r -- Resource.resource_path r
    -- putStrLn $ if isJust mi then " [Success]" else " [Failure]" :: Text
    Engine.loadTextureToAtlas (toString r) >>= \case
        Nothing -> return Nothing -- error $ "Missing resource " <> toString r
        Just !i -> return $ Just (r, i)

loadFonts :: Config -> Engine us ()
loadFonts conf = do
    let dhallPath = getDhallPath conf
    fc <- inputAuto @FontsConfig dhallPath "./Fonts.dhall"
    let fpath = fromMaybe "data/fonts" $ fc^.ff#fontsPath
    forM_ (fc^.ff#fonts) (loadFontDesc fpath)
    Engine.setDefaultFonts [fc^.ff#defaultFont] 10

loadFontDesc :: FilePath -> FontDesc -> Engine us ()
loadFontDesc fpath fd = void $ Engine.loadFontFamily (fd^.name) $ def
    & fontBase              .~ mkFontPath (fd^.ff#regular)
    & fontBold              .~ (mkFontPath <$> fd^.ff#bold)
    & fontBoldItalic        .~ (mkFontPath <$> fd^.ff#boldItalic)
    & fontItalic            .~ (mkFontPath <$> fd^.ff#italic)
    where
    mkFontPath = toString . (fpath </>)

spawnAgentToAction :: Spawn AgentTypeName EntityAction -> DirectedAction
spawnAgentToAction a = directAtWorld
    $ WorldAction_SpawnEntity (SpawnEntity_Agent $ a^.name) $ def
        & actions .~ a^.actions

spawnPassiveToAction :: Spawn PassiveTypeName EntityAction -> DirectedAction
spawnPassiveToAction a = directAtWorld
    $ WorldAction_SpawnEntity (SpawnEntity_Passive $ a^.name) $ def
        & actions .~ a^.actions

--------------------------------------------------------------------------------

initBaseSt :: Config -> EntityIndex -> Engine u St
initBaseSt conf eix = do
    gs <- initGameState conf eix
    scro <- Scroller.new $ def & set scale (gs^.gameScale)

    let prep = bool (defaultBindings<>) id $ conf^.ff#clearDefaultBindings
    let km = case buildInputKeymap $ prep $ conf^.ff#bindings of
            Left ers -> error $ unlines ers
            Right kk -> kk

    return $ St
        { field_resources  = def
        , field_inputState = set inputKeymap km defaultInputState
        , field_gameState  = gs
        , field_menuState  = def
        , field_scroller   = scro
        , field_debugFlags = def
        , field_overview   = mempty
        , field_config     = def
        , field_wires      = def
        }

initGameState :: Config -> EntityIndex -> Engine u GameState
initGameState conf eix = do
    let dhallPath = getDhallPath conf
    rs <- Runes.init =<< loadRuneSet conf
    ss <- Story.init dhallPath
    return $ GameState
        { field_entities       = eix
        , field_actions        = []
        , field_targetId       = Nothing
        , field_gameScale      = 64
        , field_frameCount     = 0
        , field_gameOverScreen = Nothing
        , field_tutorialState  = Tutorial.defaultTutorialState
        , field_storyState     = ss
        , field_systemMessages = def
        , field_runesState     = rs
        , field_mapEditorState = def
        }

