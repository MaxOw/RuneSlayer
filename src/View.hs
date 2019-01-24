module View
    ( renderView
    ) where

import Delude

import qualified Linear.Matrix as Matrix
import qualified Engine
import Engine (graphics, context, RenderAction)
import Graphics.GL

import Types (Graphics, Renderer)
import Types.Entity.Common
import Types.Entity
import Types.St
import Types.MenuState
import Types.GameState
import EntityIndex (entitiesInRange)

import GUI.GameMenu

import Focus

import Diagrams.TwoD.Transform (translate)
import Engine.Graphics.Utils (mkMatHomo2)
import Engine.Graphics

--------------------------------------------------------------------------------

renderView :: Renderer
renderView delta st = case st^.menuState of
    MainMenu -> renderMainMenu delta st
    InGame   -> renderGame     delta st

renderMainMenu :: Renderer
renderMainMenu delta st = do
    renderGame delta st -- for now
    return ()

renderGame :: Renderer
renderGame _delta st = do
    renderSetup

    -- print $ focusLocation st
    -- print $ queryLocation . entityOracle <$> focusEntity st

    let viewM = viewMatrix st
    gameProjM <- orthoProjection $ def
        & set normalization (Just Height)
        & set scale (st^.gameState.gameScale)
    let viewProjM = gameProjM !*! viewM
    Engine.draw viewProjM =<< renderEntities st

    whenJust (focusEntity st) $ \e -> do
        menuProjM <- orthoProjection $ def
            & set scale (st^.gameState.menuScale)
        Engine.draw menuProjM =<< renderGameMenu st e

    Engine.swapBuffers

renderEntities :: St -> Graphics RenderAction
renderEntities st = do
    let viewRange = () -- TODO
    let eix = st^.gameState.entities
    let es = entitiesInRange viewRange eix
    let rs = map entityRender es
    return $ Engine.renderComposition rs

--------------------------------------------------------------------------------

viewMatrix :: St -> Mat4
viewMatrix
    = fromMaybe Matrix.identity
    . fmap locationToViewMatrix
    . focusLocation

locationToViewMatrix :: Location -> Mat4
locationToViewMatrix
    = mkMatHomo2
    . flip translate mempty
    . negate
    . unwrap

renderSetup :: Graphics ()
renderSetup = do
    fitViewport
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

fitViewport :: Graphics ()
fitViewport = do
    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)

