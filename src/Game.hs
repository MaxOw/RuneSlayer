module Game
    ( initSt, endSt
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine.Graphics.Utils (delObject)
import Graphics.GL (glDeleteTextures)
import Engine (RenderAction, Img, userState, texture)
import Engine (FontName, fontBase, fontBold, fontBoldItalic, fontItalic)
import qualified Diagrams.TwoD.Transform as T
import Engine.Types (Engine)
import Engine.Graphics.Scroller (newScroller)
import Types.Config
import Types.St
import Types.Entity.Common
import Entity.Player (makePlayer)
import Types.ResourceManager
import Types.DirectedAction
import Types.Entity.Item
import Types.Entity.Unit
import EntityLike
import WorldGen
import Types.Entity.Animation
import qualified Entity.Animation as Animation

-- import qualified Resource
import qualified EntityIndex

import qualified Data.Collider as Collider
import Dhall.Utils (dhallToMap, loadDhall)

initSt :: Engine () St
initSt = do
    conf   <- loadDhall "" "Config.dhall"
    wgconf <- loadDhall "data/desc" "WorldGen.dhall"

    scro <- newScroller $ def
    eix <- EntityIndex.new $ def & size .~ (wgconf^.size)
    st <- defaultSt eix scro

    loadFontFamily "Arial"
    rs <- loadResources conf
    Engine.fullyUpdateAtlas
    Engine.setDefaultFonts ["Arial"] 10

    let world = generateWorld rs wgconf
    rnd <- makeRenderOverview world
    whenNothing_ (conf^.debugMode) $
        forM_ (world^.entities) $ \e -> EntityIndex.insert e eix

    pli <- loadDhall "data/desc" "Player.dhall"
    pid <- EntityIndex.insert (playerEntity rs pli) eix
    return $ st
        & gameState.focusId .~ Just pid
        & gameState.actions .~ testInitialActions
        & resources .~ rs
        & overview  .~ rnd
        & config    .~ conf
    where
    playerEntity rs pli = toEntity $ makePlayer rs pli
        & location .~ locM 0 0
        & collisionShape .~ Just (Collider.circle 0 0.3)

    overview = ff#overview

makeRenderOverview :: WorldGenOutput -> Engine us RenderAction
makeRenderOverview out = case out^.overviewImage of
    Nothing -> return mempty
    Just oi -> do
        img <- Engine.addImageToAtlas oi
        return $ T.scale 4 $ Engine.renderImg img

loadResources :: Config -> Engine us Resources
loadResources conf = case conf^.debugMode of
    Just _m -> do
        se <- loadDhallList "StaticTypes.dhall"
        ts <- loadDhallList "TileSets.dhall"
        return $ def
               & staticMap     .~ buildMap se
               & tileSetMap    .~ buildMap ts
    Nothing -> do
        rs <- loadAllPaths
        ss <- loadDhallMap  "Sprites.dhall"
        se <- loadDhallList "StaticTypes.dhall"
        ts <- loadDhallList "TileSets.dhall"
        is <- loadDhallList "ItemTypes.dhall"
        us <- loadDhallList "UnitTypes.dhall"
        as <- loadDhallMap  "Animations.dhall"
        let res = def
                & imgMap        .~ HashMap.fromList rs
                & spriteMap     .~ ss
                & staticMap     .~ buildMap se
                & tileSetMap    .~ buildMap ts
                & itemsMap      .~ buildMap is
                & unitsMap      .~ buildMap us
        let pr = fmap (Animation.makeAnimation res)
               $ HashMap.fromList
               $ map (over _1 AnimationName)
               $ HashMap.toList as
        return $ res
            & animationsMap .~ pr

buildMap :: (HasName x name, Eq name, Hashable name) => [x] -> HashMap name x
buildMap = HashMap.fromList . map (\x -> (x^.name, x))

loadAllPaths :: Engine us [(Text, Img)]
loadAllPaths = do
    hm <- liftIO $ dhallToMap "data/desc" "ResourcePaths.dhall"
    catMaybes <$> mapM loadResource (ordNub $ HashMap.elems hm)

loadDhallList :: FromJSON a => FilePath -> Engine us [a]
loadDhallList = fmap HashMap.elems . liftIO . dhallToMap "data/desc"

loadDhallMap :: FromJSON a => FilePath -> Engine us (HashMap Text a)
loadDhallMap = liftIO . dhallToMap "data/desc"

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

loadFontFamily :: FontName -> Engine us ()
loadFontFamily fname = void $ Engine.loadFontFamily fname $ def
    & fontBase              .~ mkFontPath fname ""
    & fontBold              .~ Just (mkFontPath fname "-Bold")
    & fontBoldItalic        .~ Just (mkFontPath fname "-Bold-Italic")
    & fontItalic            .~ Just (mkFontPath fname "-Italic")
    where
    mkFontPath n s = toString $ "data/fonts/" <> n <> s <> ".ttf"

testInitialActions :: [DirectedAction]
testInitialActions = map (directAtWorld . WorldAction_SpawnEntity)
    [ SpawnEntity_Item helmetItem
    , SpawnEntity_Item potionItem
    , SpawnEntity_Item bagItem

    , SpawnEntity_Unit (batUnit $ locM 10 6)
    , SpawnEntity_Unit (batUnit $ locM  9 6.3)
    ]
    where
    helmetItem = def
        & name     .~ (ItemTypeName "Helmet")
        & location .~ (locM 1 0)

    potionItem = def
        & name     .~ (ItemTypeName "Health Potion")
        & location .~ (locM (-1) 0.2)

    bagItem = def
        & name     .~ (ItemTypeName "Bag")
        & location .~ (locM 0 1)

    batUnit loc = def
        & name     .~ (UnitTypeName "Bat")
        & location .~ (loc)
