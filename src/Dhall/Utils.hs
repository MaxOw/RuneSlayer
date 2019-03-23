module Dhall.Utils where

import Delude
import qualified Data.Text.IO as Text
import Dhall
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Dhall.Parser (Src)
import Dhall.JSON (dhallToJSON)
import Data.Aeson

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

dhallToMap :: FromJSON x => FilePath -> FilePath -> IO (HashMap Text x)
dhallToMap rdir fpath = do
    expr <- inputExprFromFile rdir fpath
    case dhallToJSON expr of
        Left err -> print err >> return mempty
        Right vl -> case fromJSON vl of
            Error err -> putStrLn err >> return mempty
            Success v -> return v

loadDhall :: (MonadIO m, FromJSON x, Default x) => FilePath -> FilePath -> m x
loadDhall rdir fpath = liftIO $ do
    expr <- inputExprFromFile rdir fpath
    case dhallToJSON expr of
        Left err -> print err >> return def
        Right vl -> case fromJSON vl of
            Error err -> print err >> return def
            Success v -> return v

