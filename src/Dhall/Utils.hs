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

inputExprFromFile :: FilePath -> IO (Expr Src X)
inputExprFromFile fpath = inputExprWithSettings
    (set sourceName fpath defaultInputSettings)
    =<< Text.readFile fpath

dhallToMap :: FromJSON x => FilePath -> IO (Maybe (Map Text x))
dhallToMap fpath = do
    expr <- inputExprFromFile fpath
    case dhallToJSON expr of
        Left err -> print err >> return Nothing
        Right vl -> case fromJSON vl of
            Error err -> putStrLn err >> return Nothing
            Success v -> return v

