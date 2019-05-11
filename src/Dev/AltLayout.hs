module Dev.AltLayout (layoutDev) where

import Delude
import Engine
import Reload.Utils (reacquire)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- import Engine.Lens.Utils (ff)
-- import Engine.Layout.Alt.Examples
import Engine.Layout.Alt (Layout, drawLayout)

import Types.GUI
import GUI.Layout

--------------------------------------------------------------------------------

data St = St
   { field_layoutToDisplay :: Layout
   , field_layoutIx        :: Int
   } deriving (Generic)
instance Default St

handleEvent :: EventHandler St
handleEvent = \case
    EventKey Key'Q     _ _                _ -> closeWindow
    EventKey Key'Space _ KeyState'Pressed _ -> nextExample
    _ -> return ()
    where
    nextExample = do
        i <- userState.ff#layoutIx <%= limitRange . (+1)
        userState.ff#layoutToDisplay .= selectLayout i

layoutsCount :: Int
layoutsCount = Vector.length layouts

selectLayout :: Int -> Layout
selectLayout = Vector.unsafeIndex layouts . limitRange

limitRange :: Int -> Int
limitRange = max 0 . min (layoutsCount-1) . flip mod layoutsCount

layouts :: Vector Layout
layouts = fromList
    [ layout_offensiveSlotsPanel off0
    , layout_offensiveSlotsPanel off1
    -- , layout_hostilityWarning False
    , layout_statusPanel $ def
        & ff#hostilesInRange .~ True
        & ff#attackMode      %~ next
    ]
    where
    off0 = def
        & ff#slots .~ map SlotDesc [1, 0.7, 0.3, 0]
    off1 = off0
        & ff#showQuery  .~ True
        & ff#queryText  .~ "What is your favorite color?"
        & ff#answerText .~ "Blue"

render :: Renderer St
render _delta s = do
    fitViewport
    clearColorWhite
    drawLayout $ s^.ff#layoutToDisplay
    swapBuffers

layoutDev :: IO ()
layoutDev = do
    ctx <- reacquire 0 $ initWindow "Test" (800, 600)

    igniteEngine ctx $ Ignition
        { initializer  = initialize
        , eventHandler = handleEvent
        , integrator   = const $ return ()
        , renderer     = render
        , finalizer    = return ()
        }

--------------------------------------------------------------------------------

initialize :: Engine () St
initialize = do
    void $ loadFontFamily "SourceHanSerif" $ def
        & fontBase       .~ "data/fonts/SourceHanSerif-Regular.otf"

    void $ loadFontFamily "Arial" $ def
        & fontBase       .~ "data/fonts/Arial.ttf"
        & fontBold       .~ Just "data/fonts/Arial-Bold.ttf"
        & fontBoldItalic .~ Just "data/fonts/Arial-Bold-Italic.ttf"
        & fontItalic     .~ Just "data/fonts/Arial-Italic.ttf"

    return $ def
        & ff#layoutToDisplay .~ selectLayout 2
