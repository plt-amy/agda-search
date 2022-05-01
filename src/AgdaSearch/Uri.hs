{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module AgdaSearch.Uri where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.SQLite.Simple
import AgdaSearch.Populate
import AgdaSearch.Schema
import Options.Applicative
import AgdaSearch.Query

_1labFile :: Text.Text -> Int -> Text.Text
_1labFile modref offset = "https://1lab.dev/" <> modref <> ".html#" <> Text.pack (show offset)
