{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Database.Schema where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (Value, encode, ToJSON(..), FromJSON(..))
import Data.ByteString.Lazy (toStrict)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.UUID (fromString, toString)

instance FromField UUID where
    fromField f = do
        str <- fromField f
        case fromString str of
            Just uuid -> return uuid
            Nothing -> returnError ConversionFailed f "Invalid UUID format"

instance ToField UUID where
    toField = toField . toString

data QueryHistory = QueryHistory
    { queryId :: UUID
    , queryText :: Text
    , resultText :: Text
    , timestamp :: UTCTime
    } deriving (Show, Generic)

instance ToJSON QueryHistory
instance FromJSON QueryHistory
instance FromRow QueryHistory
instance ToRow QueryHistory

initDB :: IO ()
initDB = do
    conn <- open "db.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS query_history (\
        \query_id TEXT PRIMARY KEY,\
        \query_text TEXT NOT NULL,\
        \result_text TEXT NOT NULL,\
        \timestamp TIMESTAMP NOT NULL\
        \)"
    close conn

saveQuery :: Connection -> Text -> Text -> IO ()
saveQuery conn query result = do
    uuid <- nextRandom
    now <- getCurrentTime
    execute conn "INSERT INTO query_history (query_id, query_text, result_text, timestamp) VALUES (?, ?, ?, ?)"
        (uuid, query, result, now)

getQueryHistory :: Connection -> IO [QueryHistory]
getQueryHistory conn = query_ conn "SELECT query_id, query_text, result_text, timestamp FROM query_history ORDER BY timestamp DESC LIMIT 100" 