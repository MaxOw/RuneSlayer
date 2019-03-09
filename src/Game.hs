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
import Types.Entity.Player
import qualified Entity.Animation as Animation
import Entity.Container
import Entity.Item
import Entity.Unit (makeUnit, testUnitType_bat)
import GameState
import EntityLike
import WorldGen (generateWorld, genTest)

import qualified Resource
import qualified EntityIndex
import EntityIndex (EntityIndex)

import qualified Data.Collider as Collider

initSt :: Engine () St
initSt = do
    genTest
    scro <- newScroller $ def
    st <- defaultSt scro
    loadFontFamily "Arial"
    rs <- catMaybes <$> mapM loadResource
        (ordNub $ map (view path) Resource.allSprites)
    Engine.fullyUpdateAtlas
    Engine.setDefaultFonts ["Arial"] 10
    world <- generateWorld $ Size 30 30
    let eix = st^.gameState.entities
    forM_ world $ \e -> EntityIndex.insert e eix
    pid <- EntityIndex.insert playerEntity eix
    void $ EntityIndex.insert (batEntity $ locM 3 6)   eix
    void $ EntityIndex.insert (batEntity $ locM 2 6.3) eix
    setupTestGameState eix
    return $ st
        & gameState.focusId .~ Just pid
        & resources .~ HashMap.fromList rs
    where
    playerEntity = toEntity @Player $ def
        & location .~ locM 0 0
        & collisionShape .~ Just (Collider.circle 0 0.3)
        & animation      .~ Animation.characterAnimation

    batEntity atLoc = toEntity $ makeUnit testUnitType_bat
        & location .~ atLoc -- locM 3 6

endSt :: Engine St ()
endSt = do
    rs <- uses (userState.resources) HashMap.elems
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
