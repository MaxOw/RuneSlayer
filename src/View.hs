module View
    ( renderView
    ) where

import Delude

import qualified Linear.Matrix as Matrix
import qualified Engine
import Engine (graphics, context, RenderAction)
import Engine.Layout.Render (makeRenderLayout)
import Graphics.GL

import Types (Graphics, Renderer)
import Types.Entity.Common
import Types.Entity
import Types.St
import Types.MenuState
import Types.GameState
import Types.Debug
import EntityIndex (lookupInRange)
import GameState (isDebugFlagOn)

import GUI.GameMenu

import Focus

import Diagrams.TwoD.Transform (translate)
import qualified Diagrams.TwoD.Transform as T
import Engine.Graphics.Utils (mkMatHomo2)
import Engine.Graphics
import Engine.Common.Types
import Engine.Graphics.Scroller (updateScroller, makeRenderScroller)

-- import qualified Resource
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
-- import Resource (Resource)
-- import ResourceManager (lookupResource, ResourceMap)

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
    renderScroller <- prerenderUpdate st
    renderSetup

    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)

    let viewScale = st^.gameState.gameScale

    viewPos <- focusPos
    let viewportPos  = viewPos ^* (realToFrac viewScale)
    let viewportSize = Size (fromIntegral w) (fromIntegral h)

    let viewRange = mkBBoxCenter viewPos (viewportSize ^/ (realToFrac viewScale))

    zoomOutScrollerDebug   <- isDebugFlagOn DebugFlag_ZoomOutScroller
    hideScrollerDebug      <- isDebugFlagOn DebugFlag_HideScroller
    showDynamicBBoxesDebug <- isDebugFlagOn DebugFlag_ShowDynamicBoundingBoxes

    let magScale = if zoomOutScrollerDebug then 0.5 else 1

    viewM <- viewMatrix viewScale
    gameProjM <- orthoProjection $ def
        & set scale magScale
    let viewProjM = gameProjM !*! viewM

    es <- lookupInRange EntityKind_Dynamic viewRange $ st^.gameState.entities
    is <- lookupInRange EntityKind_Item    viewRange $ st^.gameState.entities
    ss <- lookupInRange EntityKind_Static  viewRange $ st^.gameState.entities

    Engine.draw viewProjM $ renderComposition
        [ mempty
        , renderUnless hideScrollerDebug renderScroller
        , T.scale viewScale $ renderComposition
            [ renderEntities (es <> is <> ss) st
            , renderIf showDynamicBBoxesDebug $ renderBBoxesDebug es st
            ]
        , renderViewportDebug zoomOutScrollerDebug viewportPos viewportSize
        ]

    whenJustM focusEntity $ \_ -> do
        menuProjM <- orthoProjection $ def
            & set scale (st^.gameState.menuScale)
        Engine.draw menuProjM =<< makeRenderLayout =<< gameMenuLayout

    Engine.swapBuffers

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
    & T.scaleX (realToFrac $ s^.width)
    & T.scaleY (realToFrac $ s^.height)
    & T.translate (fmap realToFrac p)

renderBBoxesDebug :: HasEntity e Entity => [e] -> St -> RenderAction
renderBBoxesDebug _ _ = mempty

{-
randomTilesGen :: St -> BBox Float -> RenderAction
randomTilesGen st b = renderComposition $ map (renderTile st) ps
    where
    off :: V2 Int
    off = V2 (-10) (-10) -- fmap floor $  b^.minPoint

    ps = [V2 x y | x <- take 23 [off^._x..], y <- take 23 [off^._y..]]

renderTile :: St -> V2 Int -> RenderAction
renderTile st v@(V2 x y)
    = renderSprite st tile
    & translate (fmap fromIntegral v)
    where
    p0 = 2147480011
    p1 = 2147480197
    tile = mkTile $ mod (x*p0 + y*p1 + x*y*p0*p1 + x + y*x) 3
    mkTile i = Resource.mkEnvRect (42 + i*2) 10 2 2

renderSprite :: HasResources c ResourceMap => c -> Resource -> RenderAction
renderSprite ctx r = case lookupResource r $ ctx^.resources of
    Nothing  -> renderShape shape
    Just img -> T.scale (1/32) $ renderImg img
    where
    shape = def
        & shapeType   .~ SimpleSquare
        & color       .~ Color.opaque Color.gray
-}

renderEntities :: HasEntity e Entity => [e] -> St -> RenderAction
renderEntities es st = Engine.renderComposition rs
    where
    rs = map (flip entityRender ctx . view entity) ese
    ese = sortWith f es
    f x = Down $ fromMaybe 0 $ x^?entity.oracle.location.traverse._Wrapped._y
    ctx = RenderContext
        { renderContext_resources  = st^.resources
        , renderContext_debugFlags = st^.debugFlags
        }

--------------------------------------------------------------------------------

viewMatrix :: Double -> Graphics Mat4
viewMatrix s
    = fromMaybe Matrix.identity
    . fmap locationToViewMatrix
    <$> focusLocation
    where
    locationToViewMatrix :: Location -> Mat4
    locationToViewMatrix
        = mkMatHomo2
        . flip translate mempty
        . negate
        . (^* s)
        . unwrap

--------------------------------------------------------------------------------

focusPos :: Graphics (V2 Float)
focusPos = do
    mloc <- focusLocation
    return $ fmap realToFrac $ fromMaybe 0 $ view _Wrapped <$> mloc

prerenderUpdate :: St -> Graphics RenderAction
prerenderUpdate st = do
    let s = st^.scroller
    let vscale = realToFrac $ st^.gameState.gameScale
    vpos <- focusPos
    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)
    let vsize = Size (fromIntegral w) (fromIntegral h)
    updateScroller s vscale vpos vsize $ \bb -> do
        es <- lookupInRange EntityKind_Tile bb (st^.gameState.entities)
        return $ renderEntities es st
        -- randomTilesGen st bb
    makeRenderScroller s

renderSetup :: Graphics ()
renderSetup = do
    fitViewport
    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

fitViewport :: Graphics ()
fitViewport = do
    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)
    glViewport 0 0 (fromIntegral w) (fromIntegral h)

