{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryLiteSQL.Web.Routes where

import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value, encode)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Time (getCurrentTime)

import QueryLiteSQL.Types.Env (Env(..))
import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory)

routes :: ScottyT Text (ReaderT Env IO) ()
routes = do
    -- Serve static files
    get "/" $ file "static/index.html"
    get "/app.js" $ file "static/app.js"
    get "/app.css" $ file "static/app.css"

    -- API endpoints
    post "/api/query" $ do
        query <- param "query"
        env <- ask
        
        case parseSQL query of
            Left err -> json $ object ["error" .= err]
            Right sqlQuery -> do
                result <- liftIO $ executeQuery env sqlQuery
                liftIO $ saveQuery (dbConnection env) query (decodeUtf8 $ toStrict $ encode result)
                json result

    get "/api/history" $ do
        env <- ask
        history <- liftIO $ getQueryHistory (dbConnection env)
        json history

executeQuery :: Env -> SQLQuery -> IO Value
executeQuery env query = do
    -- TODO: Implement query execution
    return $ object ["message" .= ("Query execution not implemented yet" :: String)] 