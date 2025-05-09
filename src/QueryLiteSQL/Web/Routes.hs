{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Web.Routes where

import qualified Web.Scotty.Trans as Scotty
import qualified Wai.Middleware.Static as Static
import Data.Text.Lazy (Text)
import Data.Aeson (Value(..), object, (.=))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Parser.Executor (executeQuery)
import QueryLiteSQL.Types.Env (Env(..))
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory)
import Database.SQLite.Simple (Connection)

routes :: Scotty.ScottyT Text (ReaderT Env IO) ()
routes = do
    -- Serve static files
    Scotty.middleware $ Static.staticPolicy (Static.noDots >-> Static.addBase "static")

    -- API endpoints
    Scotty.get "/api/query" $ do
        query <- Scotty.param "q"
        case parseSQL query of
            Left err -> Scotty.json $ object ["error" .= err]
            Right sqlQuery -> do
                env <- lift ask
                case executeQuery sqlQuery (jsonData env) of
                    Left err -> Scotty.json $ object ["error" .= err]
                    Right result -> do
                        conn <- liftIO $ connection env
                        liftIO $ saveQuery conn query (show result)
                        Scotty.json $ object ["result" .= result]

    Scotty.get "/api/history" $ do
        env <- lift ask
        conn <- liftIO $ connection env
        history <- liftIO $ getQueryHistory conn
        Scotty.json history

    -- Frontend routes
    Scotty.get "/" $ Scotty.file "static/index.html"
    Scotty.get "/:file" $ do
        file <- Scotty.param "file"
        Scotty.file $ "static/" ++ file 