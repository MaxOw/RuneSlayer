module Game
    ( initSt
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine (Img)
import Engine (FontName, fontBase, fontBold, fontBoldItalic, fontItalic)
import Engine.Types (Engine)
import Engine.Common.Types
import Engine.Graphics.Scroller (newScroller)
import Types.St
import Types.Entity.Common
import Types.Entity.Player
import Entity.Container
import Entity.Item
import GameState
import EntityLike
import WorldGen (generateWorld, genTest)

import qualified Resource
import qualified EntityIndex
import EntityIndex (EntityIndex)

initSt :: Engine () St
initSt = do
    genTest
    scro <- newScroller $ def
    st <- defaultSt scro
    loadFontFamily "Arial"
    rs <- catMaybes <$> mapM loadResource
        (ordNub $ map (view path) Resource.allSprites)
    Engine.fullyUpdateAtlas
    world <- generateWorld $ Size 30 30
    let eix = st^.gameState.entities
    forM_ world $ \e -> EntityIndex.insert e eix
    pid <- EntityIndex.insert playerEntity eix
    setupTestGameState eix
    return $ st
        & gameState.focusId .~ Just pid
        & resources .~ HashMap.fromList rs
    where
    playerEntity = toEntity @Player $ def
        & location .~ locM 0 0

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

setupTestGameState :: MonadIO m => EntityIndex -> m ()
setupTestGameState eix = mapM_ (flip EntityIndex.insert eix)
    [ bagEntity
    , helmetEntity
    , potionEntity
    ]
    where
    bagEntity = toEntity $ makeContainer testContainerType_bag
        & location .~ (Just $ locM 0 1)

    helmetEntity = toEntity $ makeItem testItemType_helmet
        & location .~ (Just $ locM 1 0)

    potionEntity = toEntity $ makeItem testItemType_healthPotion
        & location .~ (Just $ locM (-1) 0.2)
