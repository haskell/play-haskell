{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module DB (
    Database, ErrCode(..), KeyType, ContentsType,
    withDatabase,
    storePaste, getPaste
) where

import Control.Exception (tryJust, handleJust)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Text as T
import Database.SQLite.Simple
import System.Exit (die)
import System.IO (hPutStrLn, stderr)


maxDbFileSize :: Int
maxDbFileSize = 1024 * 1024 * 1024  -- 1 GiB

dbFileName :: FilePath
dbFileName = "pastes.db"


newtype Database = Database Connection

type KeyType = Short.ShortByteString
type ContentsType = ByteString

data ErrCode = ErrExists  -- ^ Key already exists in database
             | ErrFull    -- ^ Database disk quota has been reached
  deriving (Show)

withDatabase :: (Database -> IO ()) -> IO ()
withDatabase act =
    withConnection dbFileName $ \conn -> do
        execute_ conn "PRAGMA foreign_keys = on"
        [Only pageSize] <- query_ conn "PRAGMA page_size"
        [Only pageCount] <- query_ conn "PRAGMA page_count"

        let maxPages = max pageCount (maxDbFileSize `div` pageSize)
        when (pageCount > maxDbFileSize `div` pageSize) $
            hPutStrLn stderr "WARNING: database is already larger than limit!"

        databaseVersion (Database conn) >>= \case
            Nothing -> do
                applySchema (Database conn)
                hPutStrLn stderr $ "Created database at '" ++ dbFileName ++ "'"
            Just ver | ver == schemaVersion -> return ()
                     | otherwise -> die $ "ERROR: Incompatible database schema version (DB " ++
                                            show ver ++ ", app " ++ show schemaVersion ++ ")"

        -- SQLite can't prepare a PRAGMA statement apparently, so let's do it
        -- like this (acceptable because this is, statically, an Int)
        execute_ conn $ Query (T.pack ("PRAGMA max_page_count = " ++ show maxPages))
        act (Database conn)

schema :: [Query]
schemaVersion :: Int
(schema, schemaVersion) =
    (["CREATE TABLE meta (version INTEGER);"
     ,"CREATE TABLE pastes (key BLOB PRIMARY KEY, value BLOB);"]
    ,1)

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

storePaste :: Database -> Short.ShortByteString -> ByteString -> IO (Maybe ErrCode)
storePaste (Database conn) key value =
    let predicate (SQLError { sqlError = ErrorError }) = Just ()
        predicate _ = Nothing
    in handleJust predicate (const (return (Just ErrFull))) $
        withTransaction conn $ do
            [Only count] <- query conn "SELECT COUNT(*) FROM pastes WHERE key = ?"
                                       (Only (Short.fromShort key))
            if (count :: Int) == 0
                then do
                    execute conn "INSERT INTO pastes (key, value) VALUES (?, ?)"
                                 (Short.fromShort key, value)
                    return Nothing
                else return (Just ErrExists)

getPaste :: Database -> Short.ShortByteString -> IO (Maybe ByteString)
getPaste (Database conn) key = do
    res <- query conn "SELECT value FROM pastes WHERE key = ?" (Only (Short.fromShort key))
    case res of
        [Only value] -> return (Just value)
        [] -> return Nothing
        _ -> return Nothing  -- multiple entries, what?
