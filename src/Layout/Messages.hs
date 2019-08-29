module Layout.Messages where

import Delude
import Engine.Layout.Alt
import GUI.Layout.Common
import Types.Messages

import qualified Color

--------------------------------------------------------------------------------

layout_systemMessages :: [SystemMessage] -> Layout
layout_systemMessages [] = def
layout_systemMessages ls = lns
    & align .~ TopLeft
    & padding.each .~ 20 @@ px
    where
    lns = vrel $ map ((40 @@ px,) . msg . view content) ls
    msg x = textline ft x & align .~ TopLeft
    ft = makeFs 12 Color.black
    -- ft = makeFs 12 Color.darkslategray
