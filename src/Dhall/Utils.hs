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

dhallToMap :: FromJSON x => FilePath -> FilePath -> IO (Maybe (Map Text x))
dhallToMap rdir fpath = do
    expr <- inputExprFromFile rdir fpath
    case dhallToJSON expr of
        Left err -> print err >> return Nothing
        Right vl -> case fromJSON vl of
            Error err -> putStrLn err >> return Nothing
            Success v -> return v

