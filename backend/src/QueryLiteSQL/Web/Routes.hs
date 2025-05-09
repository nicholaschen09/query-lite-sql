{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module QueryLiteSQL.Web.Routes where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text)
import Data.Text (pack)
import Data.Aeson (Value(..), object, (.=), ToJSON(..))
import qualified Network.Wai.Middleware.Static as Static
import Database.SQLite.Simple (Connection)

-- Import scotty trans modules
import Web.Scotty.Trans

import QueryLiteSQL.Parser.SQL (parseSQL)
import QueryLiteSQL.Parser.Executor (executeQuery)
import qualified QueryLiteSQL.Types.Env as Env
import QueryLiteSQL.Database.Schema (saveQuery, getQueryHistory, QueryHistory(..))

-- Define ToJSON instance for QueryHistory
instance ToJSON QueryHistory where
    toJSON history = object 
        [ "id" .= queryId history
        , "query" .= queryText history
        , "result" .= queryResult history
        , "timestamp" .= timestamp history
        ]

-- Define type aliases for our monads
type AppM = ReaderT Env.Env IO

-- ScottyT uses the base monad as parameter
type App = ScottyT AppM
type Handler = ActionT AppM

-- Define routes for the application
routes :: App ()
routes = do
    -- Serve static files from the frontend directory
    middleware (Static.static "frontend")

    -- Route to serve the index.html file
    get "/" $ do
        file "frontend/index.html"

    -- Route to execute SQL queries
    get "/api/query" $ do
        queryText <- param "q"
        case parseSQL queryText of
            Left err -> json $ object ["error" .= pack err]
            Right sqlQuery -> do
                env <- lift ask
                let jsonData = [Env.jsonData env]  -- Convert Value to [Value] as required by executeQuery
                    conn = Env.dbConnection env
                case executeQuery sqlQuery jsonData of
                    Left err -> json $ object ["error" .= pack err]
                    Right result -> do
                        -- Convert the JSON Value to Text for storage
                        let resultText = pack $ show result
                        -- Save the query to the database
                        liftIO $ saveQuery conn queryText resultText
                        json result

    -- Route to get query history
    get "/api/history" $ do
        env <- lift ask
        let conn = Env.dbConnection env
        history <- liftIO $ getQueryHistory conn
        json history 