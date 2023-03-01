{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module DB (
  Database, ErrCode(..),
  withDatabase,
  storePaste, getPaste,
  removeExpiredPastes,
) where

import Control.Exception (tryJust, handleJust)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Database.SQLite.Simple
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import PlayHaskellTypes (Version(..), Paste(..), KeyType, ClientAddr)


maxDbFileSize :: Int
maxDbFileSize = 1024 * 1024 * 1024  -- 1 GiB

dbFileName :: FilePath -> FilePath
dbFileName dbdir = dbdir ++ "/pastes.db"


newtype Database = Database Connection

data ErrCode = ErrExists  -- ^ Key already exists in database
             | ErrFull    -- ^ Database disk quota has been reached
  deriving (Show)

withDatabase :: FilePath -> (Database -> IO ()) -> IO ()
withDatabase dbdir act =
  withConnection (dbFileName dbdir) $ \conn -> do
    execute_ conn "PRAGMA foreign_keys = on"
    [Only pageSize] <- query_ conn "PRAGMA page_size"
    [Only pageCount] <- query_ conn "PRAGMA page_count"

    let maxPages = max pageCount (maxDbFileSize `div` pageSize)
    when (pageCount > maxDbFileSize `div` pageSize) $
      hPutStrLn stderr "WARNING: database is already larger than limit!"

    databaseVersion (Database conn) >>= \case
      Nothing -> do
        applySchema (Database conn)
        hPutStrLn stderr $ "Created database at '" ++ dbFileName dbdir ++ "'"
      Just ver | ver == schemaVersion -> return ()
               | otherwise -> die $ "ERROR: Incompatible database schema version (DB " ++
                                      show ver ++ ", app " ++ show schemaVersion ++ ")"

    -- SQLite can't prepare a PRAGMA statement apparently, so let's do it
    -- like this (acceptable because this is, statically, an Int)
    execute_ conn $ Query (T.pack ("PRAGMA max_page_count = " ++ show maxPages))
    act (Database conn)

-- 'pastes.date' and 'pastes.expire' are unix timestamps.
schema :: [Query]
schemaVersion :: Int
(schema, schemaVersion) =
  (["CREATE TABLE meta (version INTEGER NOT NULL)"
   ,"CREATE TABLE pastes (\n\
    \    id INTEGER PRIMARY KEY NOT NULL, \n\
    \    key BLOB NOT NULL, \n\
    \    ghcVersion TEXT NOT NULL, \n\
    \    date INTEGER NULL, \n\
    \    expire INTEGER NULL, \n\
    \    srcip TEXT NULL, \n\
    \    parent INTEGER REFERENCES pastes (id) ON DELETE SET NULL,\n\
    \    UNIQUE (key)\n\
    \)"
   ,"CREATE UNIQUE INDEX pastes_key ON pastes (key)"
   ,"CREATE TABLE files (\n\
    \    paste INTEGER NOT NULL, \n\
    \    fname BLOB NULL, \n\
    \    value BLOB NOT NULL, \n\
    \    fileorder INTEGER NOT NULL, \n\
    \    FOREIGN KEY (paste) REFERENCES pastes (id) ON DELETE CASCADE\n\
    \)"
   ,"CREATE INDEX files_paste ON files (paste)"]
  ,5)

databaseVersion :: Database -> IO (Maybe Int)
databaseVersion (Database conn) = do
  let predicate (SQLError { sqlError = ErrorError }) = Just ()
      predicate _ = Nothing
  res <- tryJust predicate $ query_ conn "SELECT version FROM meta"
  case res of
    Right [Only ver] -> return (Just ver)
    Right [] -> die "ERROR: no version field in database meta table"
    Right _ -> die "ERROR: stop fooling around in meta"
    Left () -> return Nothing

applySchema :: Database -> IO ()
applySchema (Database conn) = do
  mapM_ (execute_ conn) schema
  execute conn "INSERT INTO meta (version) VALUES (?)" (Only schemaVersion)

storePaste :: Database -> ClientAddr -> KeyType -> Paste -> IO (Maybe ErrCode)
storePaste (Database conn) clientaddr key (Paste (Version ghcVersion) files mparent mexpire) = do
  now <- truncate <$> getPOSIXTime :: IO Int
  let mexpire' = truncate <$> mexpire :: Maybe Int
  let predicate (SQLError { sqlError = ErrorError }) = Just ()
      predicate _ = Nothing
  handleJust predicate (const (return (Just ErrFull))) $
    withTransaction conn $ do
      [Only count] <- query conn "SELECT COUNT(*) FROM pastes WHERE key = ?"
                                 (Only key)
      if (count :: Int) == 0
        then do
          case mparent of
            Just parent ->
              execute conn "INSERT INTO pastes (key, ghcVersion, date, expire, srcip, parent) \
                           \VALUES (?, ?, ?, ?, ?, (SELECT id FROM pastes WHERE key = ?))"
                           (key, ghcVersion, now, mexpire', clientaddr, parent)
            Nothing ->
              execute conn "INSERT INTO pastes (key, date, expire, srcip) \
                           \VALUES (?, ?, ?, ?)"
                           (key, now, mexpire', clientaddr)
          pasteid <- lastInsertRowId conn
          forM_ (zip files [1::Int ..]) $ \((mfname, contents), idx) ->
            execute conn "INSERT INTO files (paste, fname, value, fileorder) \
                         \VALUES (?, ?, ?, ?)"
                    (pasteid, mfname, contents, idx)
          return Nothing
        else return (Just ErrExists)

getPaste :: Database -> KeyType -> IO (Maybe (Maybe POSIXTime, Paste))
getPaste (Database conn) key = do
  res <- query @_ @(Maybe Int, String, Maybe Int, Maybe ByteString, ByteString, Maybe ByteString)
               conn "SELECT P.date, P.ghcVersion, P.expire, F.fname, F.value, (SELECT key FROM pastes WHERE id = P.parent) \
                    \FROM pastes AS P, files as F \
                    \WHERE P.id = F.paste AND P.key = ? ORDER BY F.fileorder"
               (Only key)
  case res of
    (date, ghcVersion, expire, _, _, mparent) : _ ->
      let date' = secondsToNominalDiffTime . fromIntegral <$> date
          expire' = secondsToNominalDiffTime . fromIntegral <$> expire
          files = [(mfname, contents) | (_, _, _, mfname, contents, _) <- res]
          ghcVersion' = Version ghcVersion
      in return (Just (date', Paste ghcVersion' files mparent expire'))
    [] -> return Nothing

removeExpiredPastes :: Database -> IO ()
removeExpiredPastes (Database conn) = do
  now <- truncate <$> getPOSIXTime :: IO Int
  execute conn "DELETE FROM pastes WHERE ? >= expire" (Only now)
