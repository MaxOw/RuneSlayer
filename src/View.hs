{-# Language PatternSynonyms #-}
module View
    ( renderView
    , prerenderUpdate
    ) where

import Delude hiding (context)

import qualified Linear.Matrix as Matrix
import qualified Engine
import Engine (graphics, context, RenderAction, Engine)
import Engine.Layout.Alt (Layout, makeRenderLayout, pattern BottomLeft)
-- import Engine.Debug (logOnce)
import Graphics.GL

import Types (Graphics, Renderer)
import Types.Entity.Common
import Types.Config
import Types.St
import Types.MenuState
import Types.GameState (gameState)
import Types.Debug
import Entity
import EntityIndex (lookupInRange)
import GameState (isDebugFlagOn)

import GUI.GameMenu

import Focus

import Diagrams.TwoD.Transform (translate)
import qualified Diagrams.TwoD.Transform as T
import Engine.Graphics.Utils (mkMatHomo2)
import Engine.Graphics
import Engine.Common.Types
import Engine.Graphics.Scroller (updateScroller', makeRenderScroller)
import qualified Engine.Layout.Alt as Engine (drawLayout)

import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color

--------------------------------------------------------------------------------

renderView :: Renderer
renderView delta st = case st^.config.debugMode of
    Just dm -> renderDebugMode dm delta st
    Nothing -> case st^.menuState of
        MainMenu -> renderMainMenu delta st
        InGame   -> renderGame     delta st

renderMainMenu :: Renderer
renderMainMenu delta st = do
    renderGame delta st -- for now
    return ()

renderGame :: Renderer
renderGame _delta st = do
    renderScroller <- prerenderUpdate False st
    renderSetup

    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)

    let viewScale = st^.gameState.gameScale

    viewPos <- cameraPos
    let viewportPos  = viewPos ^* viewScale
    let viewportSize = Size (fromIntegral w) (fromIntegral h)

    let viewRange = mkBBoxCenter viewPos (viewportSize ^/ viewScale)

    zoomOutScrollerDebug   <- isDebugFlagOn DebugFlag_ZoomOutScroller
    hideScrollerDebug      <- isDebugFlagOn DebugFlag_HideScroller
    showDynamicBBoxesDebug <- isDebugFlagOn DebugFlag_ShowDynamicBoundingBoxes

    let magScale = if zoomOutScrollerDebug then 0.5 else 1

    viewM <- viewMatrix viewScale
    gameProjM <- orthoProjection $ def
        & set scale magScale
    let viewProjM = gameProjM !*! viewM

    es <- lookupInRange EntityKind_Dynamic viewRange $ st^.gameState.entities
    is <- lookupInRange EntityKind_Passive viewRange $ st^.gameState.entities

    -- let esc = length es
    -- whenChanged_ esc $ \x -> putStrLn $ "Number of Dynamics in range: " <> show x

    Engine.draw viewProjM $ renderComposition
        [ mempty
        , renderUnless hideScrollerDebug renderScroller
        , T.scale viewScale $ renderComposition
            [ renderEntities (es <> is) st
            , renderIf showDynamicBBoxesDebug $ renderBBoxesDebug es st
            ]
        , renderViewportDebug zoomOutScrollerDebug viewportPos viewportSize
        -- , setZIndexAtLeast 20000 $ st^.ff#overview
        ]

    whenJustM focusEntity $ const $ drawScaledLayout magScale
        =<< overlayLayout (locationToLayout viewPos viewScale)
    whenJustM focusEntity $ const $ Engine.drawLayout =<< gameMenuLayout
    whenJustM gameOverScreenLayout Engine.drawLayout

    Engine.swapBuffers

-- this is crappy like that for now, later I'll need to fix it in Carnot
drawScaledLayout :: Float -> Layout -> Engine us ()
drawScaledLayout s l = when (s == 1.0) $ do
    projM <- orthoProjection $ def
        & boxAlign .~ BottomLeft
        -- & scale    .~ s
    draw projM =<< makeRenderLayout l

locationToLayout :: V2 Float -> Float -> Location -> V2 Float
locationToLayout camPos s loc = (v - camPos)^*s
    where v = Unwrapped loc

--------------------------------------------------------------------------------
-- Debug Render Modes

renderDebugMode :: DebugMode -> Renderer
renderDebugMode dm delta st = case dm of
    DebugMode_WorldGen -> renderWorldGen delta st
    DebugMode_Nothing  -> return ()

renderWorldGen :: Renderer
renderWorldGen _delta st = do
    renderSetup

    projM <- orthoProjection $ def
    Engine.draw projM $ T.rotate (1/4 @@ turn) $ st^.ff#overview

    Engine.swapBuffers

--------------------------------------------------------------------------------

renderIf :: Bool -> RenderAction -> RenderAction
renderIf True  x = x
renderIf False _ = mempty

renderUnless :: Bool -> RenderAction -> RenderAction
renderUnless False x = x
renderUnless True  _ = mempty

renderViewportDebug :: Bool -> V2 Float -> Size Float -> RenderAction
renderViewportDebug False _ _ = mempty
renderViewportDebug True  p s = renderShape $ def
    & shapeType .~ SimpleSquare
    & color     .~ Color.withOpacity Color.gray 0.3
    & T.scaleX (s^.width)
    & T.scaleY (s^.height)
    & T.translate p

renderBBoxesDebug :: HasEntity e Entity => [e] -> St -> RenderAction
renderBBoxesDebug _ _ = mempty

renderEntities :: HasEntity e Entity => [e] -> St -> RenderAction
renderEntities es st = Engine.renderComposition rs
    where
    rs = map (flip entityRender ctx . view entity) ese
    ese = sortWith f es
    f x = Down $ fromMaybe 0 $ x^?entity.oracleLocation.traverse._Wrapped._y
    ctx = RenderContext
        { field_resources  = st^.resources
        , field_debugFlags = st^.debugFlags
        }

--------------------------------------------------------------------------------

viewMatrix :: Float -> Graphics Mat4
viewMatrix s
    = fromMaybe Matrix.identity
    . fmap locationToViewMatrix
    <$> cameraLocation
    where
    locationToViewMatrix :: Location -> Mat4
    locationToViewMatrix
        = mkMatHomo2
        . flip translate mempty
        . negate
        . (^* s)
        . unwrap

--------------------------------------------------------------------------------

cameraPos :: Graphics (V2 Float)
cameraPos = do
    mloc <- cameraLocation
    return $ fromMaybe 0 $ view _Wrapped <$> mloc

prerenderUpdate :: Bool -> St -> Graphics RenderAction
prerenderUpdate forceRedraw st = do
    let s = st^.scroller
    let vscale = st^.gameState.gameScale
    vpos <- cameraPos
    -- (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)
    -- let vtrigsize = Size (fromIntegral w) (fromIntegral h)
    let vtrigsize = Size 2 2
    updateScroller' s forceRedraw vscale vpos vtrigsize $ \bb -> do
        es <- lookupInRange EntityKind_Tile bb (st^.gameState.entities)
        -- logOnce (show $ length es)
        return $ renderEntitiesRaw es st
        -- Engine.drawAtlasBatch projM $ renderEntitiesRaw es st
        -- return $ renderEntities (take 500 es) st
    makeRenderScroller s

-- renderEntitiesRaw :: HasEntity e Entity => [e] -> St -> Vector AtlasDesc
renderEntitiesRaw :: HasEntity e Entity => [e] -> St -> RenderAction
-- renderEntitiesRaw es st = Vector.mapMaybe f $ Vector.take 1000 $ Vector.fromList es
renderEntitiesRaw es st = renderComposition rs
    where
    rs = map (flip entityRender ctx . view entity) es
    -- f = unR . flip entityRender ctx . view entity
    ctx = RenderContext
        { field_resources  = st^.resources
        , field_debugFlags = st^.debugFlags
        }
    {-
    unR = \case
        RenderFromAtlas d -> Just d
        _ -> Nothing
    -}


renderSetup :: Graphics ()
renderSetup = do
    fitViewport
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

