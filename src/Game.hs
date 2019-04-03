module Game
    ( initSt, endSt
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine.Graphics.Utils (delObject)
import Graphics.GL (glDeleteTextures)
import Engine (Img, userState, texture)
import Engine (FontName, fontBase, fontBold, fontBoldItalic, fontItalic)
import Engine.Types (Engine)
import Engine.Common.Types
import Engine.Graphics.Scroller (newScroller)
import Types.St
import Types.Entity.Common
import Entity.Player (makePlayer)
import Types.ResourceManager
import Types.DirectedAction
import Types.Entity.ItemType
import Types.Entity.Unit
import GameState
import EntityLike
import WorldGen (generateWorld)

-- import qualified Resource
import qualified EntityIndex

import qualified Data.Collider as Collider
import Dhall.Utils (dhallToMap, loadDhall)

initSt :: Engine () St
initSt = do
    scro <- newScroller $ def
    st <- defaultSt scro
    loadFontFamily "Arial"
    rs <- loadResources
    Engine.fullyUpdateAtlas
    Engine.setDefaultFonts ["Arial"] 10
    world <- generateWorld rs $ Size 30 30
    let eix = st^.gameState.entities
    forM_ world $ \e -> EntityIndex.insert e eix
    pli <- loadDhall "data/desc" "Player.dhall"
    pid <- EntityIndex.insert (playerEntity rs pli) eix
    return $ st
        & gameState.focusId .~ Just pid
        & gameState.actions .~ testInitialActions
        & resources .~ rs
    where
    playerEntity rs pli = toEntity $ makePlayer rs pli
        & location .~ locM 0 0
        & collisionShape .~ Just (Collider.circle 0 0.3)

loadResources :: Engine us Resources
loadResources = do
    rs <- loadAllPaths
    ss <- loadDhallMap  "Sprites.dhall"
    se <- loadDhallList "StaticTypes.dhall"
    ts <- loadDhallList "TileSets.dhall"
    is <- loadDhallList "ItemTypes.dhall"
    us <- loadDhallList "UnitTypes.dhall"
    as <- loadDhallMap  "Animations.dhall"
    return $ def
        & resourceMap   .~ HashMap.fromList rs
        & spriteMap     .~ ss
        & staticMap     .~ buildMap se
        & tileSetMap    .~ buildMap ts
        & itemsMap      .~ buildMap is
        & unitsMap      .~ buildMap us
        & animationsMap .~ as

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
    rs <- uses (userState.resources.resourceMap) HashMap.elems
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

    , SpawnEntity_Unit (batUnit $ locM 3 6)
    , SpawnEntity_Unit (batUnit $ locM 2 6.3)
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
