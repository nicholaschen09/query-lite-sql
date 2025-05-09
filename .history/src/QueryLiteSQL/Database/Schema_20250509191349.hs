{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Database.Schema
    ( initDB
    , saveQuery
    , getQueryHistory
    , QueryHistory(..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import GHC.Generics (Generic)

-- Define the QueryHistory data type
data QueryHistory = QueryHistory
    { queryId :: Int
    , queryText :: Text
    , queryResult :: Text
    , timestamp :: UTCTime
    } deriving (Show, Generic)

instance FromRow QueryHistory where
    fromRow = QueryHistory <$> field <*> field <*> field <*> field

instance ToRow QueryHistory where
    toRow (QueryHistory id' query' result' timestamp') =
        toRow (id', query', result', timestamp')

-- Initialize the database
initDB :: IO ()
initDB = do
    conn <- open "db.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS queries (id INTEGER PRIMARY KEY, query TEXT, result TEXT, timestamp TIMESTAMP)"
    close conn

-- Save a query to the database
saveQuery :: Connection -> Text -> Text -> IO ()
saveQuery conn query' result' = do
    now <- getCurrentTime
    execute conn "INSERT INTO queries (query, result, timestamp) VALUES (?, ?, ?)" (query', result', now)

-- Get the query history
getQueryHistory :: Connection -> IO [QueryHistory]
getQueryHistory conn =
    query_ conn "SELECT id, query, result, timestamp FROM queries ORDER BY timestamp DESC" 