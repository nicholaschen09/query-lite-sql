{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QueryLiteSQL.Types.Env where

import Data.Aeson (Value)
import Database.SQLite.Simple (Connection, open)
import GHC.Generics (Generic)
import Data.Text (Text)

-- Environment type to hold application configuration
data Env = Env
    { jsonData :: Value             -- The JSON data to query
    , jsonFilePath :: FilePath      -- Path to the JSON file
    , dbConnection :: Connection    -- SQLite connection
    } deriving (Generic)

-- Create a new environment
mkEnv :: Value -> FilePath -> IO Env
mkEnv data' filePath = do
    conn <- open "db.db"
    return $ Env data' filePath conn 