module Entity.Utils
    ( module Utils
    , renderIf
    , correctHeight
    ) where

import Delude
import Types.Entity.Common     as Utils
import Types.Entity            as Utils
import Entity                  as Utils (makeEntity)
import Entity.Update           as Utils
import Engine.Graphics         as Utils hiding (context)
import Diagrams.TwoD.Transform as Utils hiding (scale)
import HasField                as Utils
import Engine.Graphics.Utils   as Utils (mkMatHomo2)

--------------------------------------------------------------------------------

renderIf :: Bool -> RenderAction -> RenderAction
renderIf True  a = a
renderIf False _ = mempty

correctHeight :: Transformable2D t => t -> t
correctHeight = translateY 0.8 -- TODO: this souldn't be hardcoded
