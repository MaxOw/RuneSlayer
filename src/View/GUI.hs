module GUI.Inventory
    ( renderInventory
    ) where

import Delude

import Engine
import Engine.Layout.Render
import Engine.Layout.Types
import Engine.Graphics.Types (RenderAction)

import Types

import qualified GUI.Style as Style

renderInventory :: St -> Graphics RenderAction
renderInventory _st = do
    rend <- makeRenderLayout $ Style.baseMenuBox opts
        [ Style.baseText "Test" ]
    return rend
    where
    opts = def
         & size .~ Size (0.8 @@ wpct) (0.8 @@ wpct)

