{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module AgdaSearch.Cli where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text.IO as Text
import Database.SQLite.Simple
import AgdaSearch.Populate
import AgdaSearch.Schema
import Options.Applicative
import System.IO
import qualified AgdaSearch.Server as Server
import AgdaSearch.Query
import AgdaSearch.Uri

data Command = CreateDB
             | CreateDBAndQuery
             | Serve
             deriving Eq

commandReader :: ReadM Command
commandReader = eitherReader $
  \case
    "createdb" -> pure CreateDB
    "createdb-and-query" -> pure CreateDBAndQuery
    "serve" -> pure Serve
    else' -> Left $ "Unkown option: '" <> else' <> "'"


data Options = MkOptions
  { oBasePath :: FilePath
  , oMain     :: FilePath
  , oDatabase :: FilePath
  , oCommand  :: Command
  }


options :: Parser Options
options = MkOptions
  <$> strArgument (metavar "BASE_PATH" <> help "The basepath for the agda compiler to run in")
  <*> strArgument (metavar "MAIN"      <> help "An agda file that imports all searchable modules")
  <*> strArgument (metavar "DB"        <> help "sqlite db file, should remain the same for caching")
  <*> (option commandReader (help "command, either createdb or createdb-and-query, defaulting to the latter" <> long "command") <|> pure CreateDBAndQuery)

readOptions :: IO Options
readOptions = customExecParser (prefs showHelpOnError) $ info
  (helper <*> options)
  (fullDesc <> header "agda-search" <> progDesc
    "A search for agda functions"
  )

-- | cabal run agda-search -- ../agda/std-lib/ ../agda/std-lib/src/All.agda db.x
--
--  to create the all.agda file:
--  download std-lib
--  run:
--
--  @
-- std-lib/src on  HEAD (b85c5d1)
-- ❯ fd ".*agda" . > All.agda
--  @
--
--  then open in vim.
--  :%s/[a-z]/import &/g
--  :%s/\.agda//g
--  :%s/\///g
main :: IO ()
main = do
  opts <- readOptions
  if oCommand opts == Serve then
    Server.serve $ oDatabase opts
  else runCli opts

runCli :: Options -> IO ()
runCli MkOptions{..} = do
  withConnection oDatabase $ \conn -> do
    createDb conn
    [Only count] <- query_ conn "select count(*) from identifiers"

    when (count == (0 :: Int)) $ do
      putStrLn "Loading types..."
      runAgda oBasePath $ do
        iss <- findInScopeSet oMain

        liftIO . putStrLn $ "Type checked! Populating database..."
        traverse_ (insertIdentifier conn) iss

    putStrLn "Loaded types!"
    putStr "\27[2J\27[H"
    when (oCommand == CreateDBAndQuery) $ forever $ do
      putStr "> "
      hFlush stdout
      queryStr <- getLine

      sl <- getIdentifiers conn queryStr

      let showit (Identifier {name = name', typestr = typestr', fileref = fileref', byteoffset = bo}) = do
            Just modref <- getModRef conn fileref'
            let url = _1labFile (modname modref) bo
            pure $ name' <> " : " <> typestr' <> "\nDefined at: " <> url

      traverse_ (Text.putStrLn <=< showit) sl
