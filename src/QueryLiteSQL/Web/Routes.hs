{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Web.Routes where

import Web.Scotty.Trans
import Data.Text.Lazy (Text)
import Data.Aeson (Value(..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Parser.Executor (executeQuery)
import QueryLiteSQL.Types.Env (Env(..))
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory)
import Database.SQLite.Simple (Connection)

routes :: ScottyT Text (ReaderT Env IO) ()
routes = do
    -- Serve static files
    middleware $ staticPolicy (noDots >-> addBase "static")

    -- API endpoints
    get "/api/query" $ do
        query <- param "q"
        case parseSQL query of
            Left err -> json $ object ["error" .= err]
            Right sqlQuery -> do
                env <- lift ask
                case executeQuery sqlQuery (jsonData env) of
                    Left err -> json $ object ["error" .= err]
                    Right result -> do
                        conn <- liftIO $ connection env
                        liftIO $ saveQuery conn query (show result)
                        json $ object ["result" .= result]

    get "/api/history" $ do
        env <- lift ask
        conn <- liftIO $ connection env
        history <- liftIO $ getQueryHistory conn
        json history

    -- Frontend routes
    get "/" $ file "static/index.html"
    get "/:file" $ do
        file <- param "file"
        file $ "static/" ++ file 