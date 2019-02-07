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
import EntityIndex (entitiesInRange)

import GUI.GameMenu

import Focus

import Diagrams.TwoD.Transform (translate)
import qualified Diagrams.TwoD.Transform as T
import Engine.Graphics.Utils (mkMatHomo2)
import Engine.Graphics
import Engine.Common.Types
import Engine.Graphics.Scroller (updateScroller, makeRenderScroller)

import qualified Resource
import qualified Data.Colour       as Color
import qualified Data.Colour.Names as Color
import Resource (Resource)
import ResourceManager (lookupResource, ResourceMap)

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

    let viewScale = 64
    let viewM = viewMatrix viewScale st
    gameProjM <- orthoProjection $ def
        -- & set normalization (Just Height)
        -- & set scale viewScale -- 1 -- (st^.gameState.gameScale)
    let viewProjM = gameProjM !*! viewM
    -- let lc = fromMaybe def $ focusLocation st
    -- let rc = Rect (pure (-11) + (realToFrac <$> lc^._Wrapped)) (pure 10)
    -- Engine.draw viewProjM $ T.scale viewScale $ renderComposition
    Engine.draw viewProjM $ renderComposition
        [ mempty
        , renderScroller
        -- , randomTilesGen st rc
        , T.scale viewScale $ renderEntities st
        ]

    whenJust (focusEntity st) $ \e -> do
        menuProjM <- orthoProjection $ def
            & set scale (st^.gameState.menuScale)
        Engine.draw menuProjM =<< makeRenderLayout (gameMenuLayout st e)

    Engine.swapBuffers

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

renderEntities :: St -> RenderAction
renderEntities st = Engine.renderComposition rs
    where
    viewRange = () -- TODO
    eix = st^.gameState.entities
    es = entitiesInRange viewRange eix
    rs = map (flip entityRender ctx) es
    ctx = RenderContext
        { renderContext_resources = st^.resources
        }

--------------------------------------------------------------------------------

viewMatrix :: Double -> St -> Mat4
viewMatrix s
    = fromMaybe Matrix.identity
    . fmap locationToViewMatrix
    . focusLocation
    where
    locationToViewMatrix :: Location -> Mat4
    locationToViewMatrix
        = mkMatHomo2
        . flip translate mempty
        . negate
        . (^* s)
        . unwrap

--------------------------------------------------------------------------------

prerenderUpdate :: St -> Graphics RenderAction
prerenderUpdate st = do
    let s = st^.scroller
    let vscale = realToFrac $ st^.gameState.gameScale
    let vpos = fmap realToFrac $ fromMaybe 0 $ view _Wrapped <$> focusLocation st
    (w, h) <- Engine.getFramebufferSize =<< use (graphics.context)
    let vsize = Size (fromIntegral w) (fromIntegral h)
    updateScroller s vscale vpos vsize $ \bb -> do
        randomTilesGen st bb
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

