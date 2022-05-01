{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module AgdaSearch.Schema where

import Data.Text (Text)
import Database.SQLite.Simple

createDb :: Connection -> IO ()
createDb conn = do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS filemod (       \
    \ fileid INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ filepath TEXT NOT NULL,                   \
    \ modname TEXT NOT NULL                     \
    \ )"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS identifiers (          \
    \   identid    INTEGER PRIMARY KEY AUTOINCREMENT,  \
    \   name       TEXT NOT NULL,                      \
    \   typestr    TEXT NOT NULL,                      \
    \   byteoffset INTEGER NOT NULL,                   \
    \   fileref    INTEGER NOT NULL,                   \
    \   FOREIGN KEY(fileref) REFERENCES filemod(filed) \
    \ )"

data ModRef = ModRef
  { modid :: Int,
    modname :: Text,
    filepath :: FilePath
  } deriving Show

data Identifier = Identifier
  { identid :: Int,
    name :: Text,
    typestr :: Text,
    byteoffset :: Int,
    fileref :: Int
  }
  deriving (Show)

instance FromRow Identifier where
  fromRow = Identifier <$> field <*> field <*> field <*> field <*> field

instance FromRow ModRef where
  fromRow = ModRef <$> field <*> field <*> field
