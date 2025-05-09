{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- This module is now empty since we moved the routes directly into Main.hs
module QueryLiteSQL.Web.Routes where

import Web.Scotty.Trans
import qualified Network.Wai.Middleware.Static as Static
import Data.Text.Lazy (Text)
import Data.Text (pack)
import Data.Aeson (Value(..), object, (.=), ToJSON(..))
import Control.Monad.Reader (ReaderT, ask, MonadReader)
import Control.Monad.IO.Class (liftIO, MonadIO)
import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Parser.Executor (executeQuery)
import qualified QueryLiteSQL.Types.Env as Env
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory, QueryHistory(..))
import Database.SQLite.Simple (Connection)

-- Define ToJSON instance for QueryHistory
instance ToJSON QueryHistory where
    toJSON h = object [ "id" .= QueryLiteSQL.Database.Schema.id h
                       , "query" .= query h
                       , "result" .= result h
                       , "timestamp" .= timestamp h
                       ]

-- Define routes for the application
routes :: (MonadIO m, MonadReader Env.Env m) => ScottyT Text m ()
routes = do
    -- Serve static files
    middleware $ Static.staticPolicy (Static.noDots Static.>-> Static.addBase "static")

    -- API endpoints
    get "/api/query" $ do
        query <- param "q"
        case parseSQL query of
            Left err -> json $ object ["error" .= err]
            Right sqlQuery -> do
                env <- lift ask
                case executeQuery sqlQuery [Env.jsonData env] of
                    Left err -> json $ object ["error" .= err]
                    Right result -> do
                        conn <- return $ Env.dbConnection env
                        liftIO $ saveQuery conn query (pack $ show result)
                        json $ object ["result" .= result]

    get "/api/history" $ do
        env <- lift ask
        conn <- return $ Env.dbConnection env
        history <- liftIO $ getQueryHistory conn
        json $ object ["history" .= history]

    -- Frontend routes
    get "/" $ file "static/index.html"
    get "/:file" $ do
        f <- param "file"
        file $ "static/" ++ f 