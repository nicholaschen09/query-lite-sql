{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Parser.SQL where

import Data.Attoparsec.Text as A
import Data.Text (Text)
import Data.Aeson (Value(..))
import GHC.Generics (Generic)
import Control.Applicative ((<|>), many, optional)
import Data.Char (isSpace)

data SQLQuery = SQLQuery
    { selectCols :: [Text]
    , fromTable :: Text
    , whereClause :: Maybe WhereClause
    , limitClause :: Maybe Int
    } deriving (Show, Generic)

data WhereClause = WhereClause
    { conditions :: Condition
    } deriving (Show, Generic)

data Condition = And Condition Condition
               | Or Condition Condition
               | BinaryOp Text Op Value
               | Parens Condition
               deriving (Show, Generic)

data Op = Eq | Neq | Lt | Gt
        deriving (Show, Generic)

parseSQL :: Text -> Either String SQLQuery
parseSQL = parseOnly sqlQuery

sqlQuery :: Parser SQLQuery
sqlQuery = do
    string "SELECT" >> skipSpace
    cols <- selectColumns
    skipSpace >> string "FROM" >> skipSpace
    table <- takeWhile1 (not . isSpace)
    whereClause <- optional (skipSpace >> whereParser)
    limitClause <- optional (skipSpace >> limitParser)
    endOfInput
    return $ SQLQuery cols table whereClause limitClause

selectColumns :: Parser [Text]
selectColumns = do
    cols <- sepBy1 columnName (char ',' >> skipSpace)
    return cols

columnName :: Parser Text
columnName = do
    skipSpace
    name <- takeWhile1 (not . (`elem` [',', ' ', '\t', '\n']))
    return name

whereParser :: Parser WhereClause
whereParser = do
    string "WHERE" >> skipSpace
    WhereClause <$> conditionParser

conditionParser :: Parser Condition
conditionParser = do
    c <- term
    rest <- many $ do
        skipSpace
        op <- (string "AND" >> return And) <|> (string "OR" >> return Or)
        skipSpace
        c2 <- term
        return (op, c2)
    return $ foldl (\acc (op, c2) -> op acc c2) c rest

term :: Parser Condition
term = do
    skipSpace
    choice
        [ Parens <$> (char '(' >> conditionParser <* char ')')
        , binaryOpParser
        ]

binaryOpParser :: Parser Condition
binaryOpParser = do
    col <- takeWhile1 (not . (`elem` ['=', '!', '<', '>', ' ']))
    skipSpace
    op <- choice
        [ string "=" >> return Eq
        , string "!=" >> return Neq
        , string "<" >> return Lt
        , string ">" >> return Gt
        ]
    skipSpace
    val <- valueParser
    return $ BinaryOp col op val

valueParser :: Parser Value
valueParser = do
    skipSpace
    choice
        [ string "true" >> return (Bool True)
        , string "false" >> return (Bool False)
        , Number <$> scientific
        , String <$> (char '\'' *> A.takeWhile (/= '\'') <* char '\'')
        ]

limitParser :: Parser Int
limitParser = do
    string "LIMIT" >> skipSpace
    decimal 