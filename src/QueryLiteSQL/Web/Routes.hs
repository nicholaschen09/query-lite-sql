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
import QueryLiteSQL.Types.Env (Env(..))
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory, QueryHistory(..))
import Database.SQLite.Simple (Connection)

-- Define routes for the application
-- We omit the explicit type signature to let Haskell infer it
routes = do
    -- Serve static files
    middleware $ Static.staticPolicy (Static.noDots Static.>-> Static.addBase "static")

    -- API endpoints
    get "/api/query" $ do
        query <- param "q"
        case parseSQL query of
            Left err -> json $ object ["error" .= err]
            Right sqlQuery -> do
                env <- liftAndCatchIO $ ask
                case executeQuery sqlQuery [jsonData env] of
                    Left err -> json $ object ["error" .= err]
                    Right result -> do
                        conn <- return $ dbConnection env
                        liftIO $ saveQuery conn query (pack $ show result)
                        json $ object ["result" .= result]

    get "/api/history" $ do
        env <- liftAndCatchIO $ ask
        conn <- return $ dbConnection env
        history <- liftIO $ getQueryHistory conn
        json $ object ["history" .= history]

    -- Frontend routes
    get "/" $ file "static/index.html"
    get "/:file" $ do
        f <- param "file"
        file $ "static/" ++ f 