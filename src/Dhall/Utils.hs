module Dhall.Utils where

import Delude
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Dhall
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Dhall.Parser (Src)
import Dhall.JSON
import Data.Aeson
import Dhall.Instances ()

--------------------------------------------------------------------------------

inputAutoSettings :: (Interpret t, MonadIO m)
    => InputSettings -> FilePath -> Text -> m t
inputAutoSettings ses rdir
    = liftIO . inputWithSettings ss (autoWith customDhallOpts)
    where
    prdr = "./" <> rdir
    -- full = prdr <> "/" <> fpath
    ss = ses & rootDirectory .~ prdr

inputAuto :: (Interpret t, MonadIO m) => FilePath -> Text -> m t
inputAuto = inputAutoSettings defaultInputSettings

customDhallOpts :: InterpretOptions
customDhallOpts = defaultInterpretOptions
    { fieldModifier       = stripPrefix
    , constructorModifier = stripPrefix }
    where
    stripPrefix x
        | Text.take 1 x == "_" || Text.all (/='_') x = x
        | otherwise = Text.drop 1 $ Text.dropWhile (/= '_') x

--------------------------------------------------------------------------------

inputExprFromFile :: FilePath -> FilePath -> IO (Expr Src X)
inputExprFromFile rdir fpath = inputExprWithSettings opts
    =<< Text.readFile full
    where
    prdr = "./" <> rdir
    full = prdr <> "/" <> fpath
    opts = defaultInputSettings
        & rootDirectory .~ prdr
        & sourceName    .~ full

dhallToJSONConv :: Expr s X -> Either CompileError Value
dhallToJSONConv expr = dhallToJSON (convertToHomogeneousMaps conv expr)
    where
    conv = Conversion "mapKey" "mapValue"

dhallToMap :: FromJSON x => FilePath -> FilePath -> IO (HashMap Text x)
dhallToMap rdir fpath = do
    expr <- inputExprFromFile rdir fpath
    case dhallToJSONConv expr of
        Left err -> error (show err)
        Right vl -> case fromJSON vl of
            Error err -> error $ toText (fpath <> ": " <> err)
            Success v -> return v

loadDhall :: (MonadIO m, FromJSON x, Default x) => FilePath -> FilePath -> m x
loadDhall rdir fpath = liftIO $ do
    expr <- inputExprFromFile rdir fpath
    case dhallToJSONConv expr of
        Left err -> error (show err)
        Right vl -> case fromJSON vl of
            Error err -> error (toText err)
            Success v -> return v

