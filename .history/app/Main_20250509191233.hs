{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.Cors (simpleCors)
import Web.Scotty.Trans (scottyT, middleware)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import QueryLiteSQL.Types.Env (Env(..), mkEnv)
import QueryLiteSQL.Web.Routes (routes)
import QueryLiteSQL.Database.Schema (initDB)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [jsonFile] -> do
            -- Initialize database
            initDB
            
            -- Load and parse JSON data
            jsonData <- eitherDecodeFileStrict jsonFile
            case jsonData of
                Left err -> do
                    putStrLn $ "Error parsing JSON file: " ++ err
                    exitFailure
                Right data' -> do
                    -- Create environment
                    env <- mkEnv data' jsonFile
                    
                    -- Start web server
                    putStrLn $ "Starting server on port 3000..."
                    scottyT 3000 (`runReaderT` env) $ do
                        middleware simpleCors
                        routes
        _ -> do
            putStrLn "Usage: query-lite-sql <json-file>"
            exitFailure 