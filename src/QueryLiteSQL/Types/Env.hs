{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QueryLiteSQL.Types.Env where

import Data.Aeson (Value)
import Database.SQLite.Simple (Connection, open)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)

data Env = Env
    { jsonData :: Value
    , jsonFilePath :: FilePath
    , dbConnection :: Connection
    } deriving (Generic)

mkEnv :: Value -> FilePath -> IO Env
mkEnv data' filePath = do
    conn <- open "db.db"
    return $ Env data' filePath conn 