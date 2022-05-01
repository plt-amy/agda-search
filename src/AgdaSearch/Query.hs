{-# LANGUAGE OverloadedStrings #-}

module AgdaSearch.Query(getIdentifiers, getModRef) where

import qualified Data.Text as Text
import Database.SQLite.Simple
import AgdaSearch.Schema

getIdentifiers :: Connection -> String -> IO [Identifier]
getIdentifiers conn queryStr = do
        query
          conn
          "select * from identifiers where name like ?"
          (Only (Text.singleton '%' <> Text.pack queryStr <> Text.singleton '%'))

getModRef :: Connection -> Int -> IO (Maybe ModRef)
getModRef conn fileref' = do
          res <- query conn "select * from filemod where fileid = ?" (Only fileref')
          pure $ case res of
            x : _ ->  Just x
            _ -> Nothing
