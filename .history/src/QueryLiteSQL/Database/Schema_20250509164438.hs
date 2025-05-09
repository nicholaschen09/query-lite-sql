{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Database.Schema where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy (toStrict)
import Data.UUID (UUID)

data QueryHistory = QueryHistory
    { queryId :: Text
    , queryText :: Text
    , queryResult :: Text
    , queryTime :: UTCTime
    } deriving (Show, Generic)

instance FromRow QueryHistory
instance ToRow QueryHistory

initDB :: IO ()
initDB = do
    conn <- open "db.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS query_history (\
        \id TEXT PRIMARY KEY,\
        \query_text TEXT NOT NULL,\
        \query_result TEXT NOT NULL,\
        \query_time TIMESTAMP NOT NULL\
        \)"
    close conn

saveQuery :: Connection -> Text -> Text -> IO ()
saveQuery conn queryText queryResult = do
    now <- getCurrentTime
    let queryId = UUID.toString $ UUID.nextRandom
    execute conn "INSERT INTO query_history (id, query_text, query_result, query_time) VALUES (?, ?, ?, ?)"
        (queryId, queryText, queryResult, now)

getQueryHistory :: Connection -> IO [QueryHistory]
getQueryHistory conn = query_ conn "SELECT * FROM query_history ORDER BY query_time DESC LIMIT 100" 