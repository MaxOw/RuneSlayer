module Game
    ( initSt
    ) where

import Delude
import qualified Data.HashMap.Strict as HashMap
import qualified Engine
import Engine (Img)
import Engine (FontName, fontBase, fontBold, fontBoldItalic, fontItalic)
import Engine.Types (Engine)
import Types.St
import Types.Entity.Common
import Types.Entity.Player
import Types.Entity.Wall
import Entity.Container
import Entity.Item
import GameState
import EntityLike

import qualified Resource

-- import GameState

initSt :: Engine () St
initSt = do
    st <- defaultSt
    loadFontFamily "Arial"
    rs <- catMaybes <$> mapM loadResource
        (ordNub $ map (view path) Resource.allSprites)
    Engine.fullyUpdateAtlas
    return $ st
        & gameState %~ setupTestGameState
        & resources .~ HashMap.fromList rs

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

setupTestGameState :: GameState -> GameState
setupTestGameState
    = addEntityAndFocus playerEntity
    . flip (foldr addEntity)
        [ wallEntity
        , bagEntity
        , helmetEntity
        , potionEntity
        ]
    where
    playerEntity = toEntity @Player $ def
        & location .~ locM 0 0

    wallEntity = toEntity @Wall $ def
        & location .~ locM 0 3
        & health   .~ Health 100

    bagEntity = toEntity $ makeContainer testContainerType_bag
        & location .~ (Just $ locM 0 1)

    helmetEntity = toEntity $ makeItem testItemType_helmet
        & location .~ (Just $ locM 1 0)

    potionEntity = toEntity $ makeItem testItemType_healthPotion
        & location .~ (Just $ locM (-1) 0.2)
