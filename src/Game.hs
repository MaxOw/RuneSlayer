module Game
    ( initSt
    ) where

import Delude
import qualified Engine
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

-- import GameState

initSt :: Engine () St
initSt = do
    st <- defaultSt
    loadFontFamily "Arial"
    return $ over gameState setupTestGameState st

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
        & location .~ (Just $ locM 2 3)

    helmetEntity = toEntity $ makeItem testItemType_helmet
        & location .~ (Just $ locM 4 1)

    potionEntity = toEntity $ makeItem testItemType_healthPotion
        & location .~ (Just $ locM 3 1)
