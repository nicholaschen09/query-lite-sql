{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module QueryLiteSQL.Web.Routes where

import Web.Scotty.Trans
import qualified Network.Wai.Middleware.Static as Static
import Data.Text.Lazy (Text)
import Data.Text (pack)
import Data.Aeson (Value(..), object, (.=), ToJSON(..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Parser.Executor (executeQuery)
import QueryLiteSQL.Types.Env (Env(..))
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory, QueryHistory(..))
import Database.SQLite.Simple (Connection)

-- This function doesn't need a complex type - it's just a value used in a do block in main
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
                case executeQuery sqlQuery [QueryLiteSQL.Types.Env.jsonData env] of
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