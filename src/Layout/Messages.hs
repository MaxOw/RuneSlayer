module Layout.Messages where

import Delude
import Engine.Layout.Alt
import GUI.Layout.Common
import Types.Messages
import Types.Entity.Common

import qualified Color

--------------------------------------------------------------------------------

layout_infoMessages :: [SystemMessage] -> Layout
layout_infoMessages [] = def
layout_infoMessages ls = lns
    & align .~ TopLeft
    & padding.each .~ 20 @@ px
    where
    lns = vrel $ map ((40 @@ px,) . msg . view content) ls
    msg x = textline ft x & align .~ TopLeft
    ft = makeFs 12 Color.black
    -- ft = makeFs 12 Color.darkslategray

layout_locatedMessages :: (Location -> V2 Float) -> [SystemMessage] -> Layout
layout_locatedMessages conv = composition . mapMaybe go
    where
    go x = case x^.ff#messageKind of
        MessageKind_Info      -> Nothing
        MessageKind_HitEffect -> hitEffect x

    locInPixels loc = fmap ((@@ px) . realToFrac) . conv <$> loc
    maybeLocate loc f = flip absolute f <$> locInPixels loc
    msg x = textline ft x & align .~ Center
    ft = makeFs 12 Color.red

    hitEffect x = maybeLocate loc $ msg $ x^.content
        where
        tp = x^.ff#maxDuration - x^.duration
        loc = over (_Just._Wrapped._y) (+ Unwrapped tp) (x^.location)


