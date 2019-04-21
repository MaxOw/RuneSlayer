{-# Language TemplateHaskell #-}
module Version where

import Relude
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Builder
import qualified Data.Text as Text

import qualified Distribution.PackageDescription.TH as Cabal
import qualified Git.Embed                          as Git
import qualified System.Executable.Hash             as Exe

--------------------------------------------------------------------------------

executableHash :: Text
executableHash = unsafePerformIO $ do
    res <- Exe.computeExecutableHash
    return $ fromMaybe ""
        (fmap (decodeUtf8 . toLazyByteString . byteStringHex) res)

author :: Text
author = $(Cabal.packageVariable (Cabal.packageString . Cabal.author))

name :: Text
name = $(Cabal.packageVariable (Cabal.pkgName . Cabal.package))

version :: Text
version = $(Cabal.packageVariable (Cabal.pkgVersion . Cabal.package))

revision :: Text
revision = fromString $(Git.embedGitRevision)

commitTimestamp :: Text
commitTimestamp = fromString $(Git.embedGit ["log", "-n1", "--format=%ai"])

commitYear :: Text
commitYear = Text.takeWhile (/='-') commitTimestamp

copyright :: Text
copyright = "Copyright (C) 2018-" <> commitYear <> " " <> author

infoFull :: Text
infoFull = Text.unlines
    [ name <> " version " <> version <> " " <> copyright
    , "Timestamp : " <> commitTimestamp
    , "Revision  : " <> revision
    , "Exe hash  : " <> executableHash
    ]
