{-# LANGUAGE OverloadedStrings #-}

module QueryLiteSQL.Parser.SQL
    ( parseSQL
    , SQLQuery(..)
    , SQLSelect(..)
    , SQLWhere(..)
    , Condition(..)
    , BinaryOp(..)
    , Value(..)
    ) where

import Control.Applicative ((<|>), many, optional)
import Data.Attoparsec.Text
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Data.Text (Text, pack)
import qualified Data.Text as T

-- SQL Values
data Value = StringVal Text | NumberVal Double | ColumnRef Text
    deriving (Show, Eq)

-- Binary operators
data BinaryOp = Equals | NotEquals | LessThan | GreaterThan
    deriving (Show, Eq)

-- Conditions for WHERE clause
data Condition
    = BinaryCondition Text BinaryOp Value
    | AndCondition Condition Condition
    | OrCondition Condition Condition
    | Parenthesized Condition
    deriving (Show, Eq)

-- WHERE clause
data SQLWhere = SQLWhere Condition | NoWhere
    deriving (Show, Eq)

-- SELECT statement
data SQLSelect = SQLSelectColumns [Text] | SQLSelectAll
    deriving (Show, Eq)

-- Full SQL query
data SQLQuery = SQLQuery
    { sqlSelect :: SQLSelect
    , sqlWhere  :: SQLWhere
    , sqlLimit  :: Maybe Int
    }
    deriving (Show, Eq)

-- Main parsing function
parseSQL :: Text -> Either String SQLQuery
parseSQL = parseOnly sqlQueryParser

-- Parser for a full SQL query
sqlQueryParser :: Parser SQLQuery
sqlQueryParser = do
    skipSpace
    _ <- caseInsensitiveString "SELECT"
    skipSpace
    selectPart <- selectParser
    skipSpace
    _ <- caseInsensitiveString "FROM"
    skipSpace
    -- We don't really care about the table name as we only have one table
    _ <- tableNameParser
    skipSpace
    whereClause <- option NoWhere whereParser
    skipSpace
    limit <- option Nothing limitParser
    skipSpace
    endOfInput
    return $ SQLQuery selectPart whereClause limit

-- Parser for the SELECT part
selectParser :: Parser SQLSelect
selectParser = selectAllParser <|> selectColumnsParser
  where
    selectAllParser = char '*' $> SQLSelectAll
    selectColumnsParser = SQLSelectColumns <$> columnParser `sepBy1` (skipSpace *> char ',' <* skipSpace)

-- Parse column names
columnParser :: Parser Text
columnParser = pack <$> many1 (satisfy isValidColumnChar)
  where
    isValidColumnChar c = isAlphaNum c || c == '_'

-- Parse table name
tableNameParser :: Parser Text
tableNameParser = pack <$> many1 (satisfy isValidTableChar)
  where
    isValidTableChar c = isAlphaNum c || c == '_'

-- Parser for the WHERE clause
whereParser :: Parser SQLWhere
whereParser = do
    _ <- caseInsensitiveString "WHERE"
    skipSpace
    SQLWhere <$> conditionParser

-- Parser for conditions in WHERE clause
conditionParser :: Parser Condition
conditionParser = orConditionParser

-- Parser for OR conditions
orConditionParser :: Parser Condition
orConditionParser = do
    left <- andConditionParser
    option left $ do
        skipSpace
        _ <- caseInsensitiveString "OR"
        skipSpace
        right <- orConditionParser
        return $ OrCondition left right

-- Parser for AND conditions
andConditionParser :: Parser Condition
andConditionParser = do
    left <- atomicConditionParser
    option left $ do
        skipSpace
        _ <- caseInsensitiveString "AND"
        skipSpace
        right <- andConditionParser
        return $ AndCondition left right

-- Parser for atomic conditions
atomicConditionParser :: Parser Condition
atomicConditionParser = parenthesizedConditionParser <|> binaryConditionParser

-- Parser for conditions in parentheses
parenthesizedConditionParser :: Parser Condition
parenthesizedConditionParser = do
    skipSpace
    _ <- char '('
    skipSpace
    condition <- conditionParser
    skipSpace
    _ <- char ')'
    return $ Parenthesized condition

-- Parser for binary conditions
binaryConditionParser :: Parser Condition
binaryConditionParser = do
    skipSpace
    column <- columnParser
    skipSpace
    op <- binaryOpParser
    skipSpace
    value <- valueParser
    return $ BinaryCondition column op value

-- Parser for binary operators
binaryOpParser :: Parser BinaryOp
binaryOpParser =
        (string "="  $> Equals)
    <|> (string "!=" $> NotEquals)
    <|> (string "<"  $> LessThan)
    <|> (string ">"  $> GreaterThan)

-- Parser for values
valueParser :: Parser Value
valueParser = stringValueParser <|> numberValueParser <|> columnRefParser

-- Parser for string values
stringValueParser :: Parser Value
stringValueParser = do
    _ <- char '\''
    value <- many (satisfy (/= '\''))
    _ <- char '\''
    return $ StringVal (pack value)

-- Parser for number values
numberValueParser :: Parser Value
numberValueParser = do
    neg <- option "" (string "-")
    intPart <- many1 (satisfy isDigit)
    fracPart <- option "" $ do
        _ <- char '.'
        frac <- many1 (satisfy isDigit)
        return $ '.' : frac
    let numStr = neg ++ intPart ++ fracPart
    return $ NumberVal (read numStr)

-- Parser for column references
columnRefParser :: Parser Value
columnRefParser = ColumnRef <$> columnParser

-- Parser for LIMIT clause
limitParser :: Parser (Maybe Int)
limitParser = do
    _ <- caseInsensitiveString "LIMIT"
    skipSpace
    Just . read <$> many1 (satisfy isDigit)

-- Case insensitive string parser
caseInsensitiveString :: String -> Parser Text
caseInsensitiveString str = do
    res <- mapM caseInsensitiveChar str
    return $ pack res

-- Case insensitive character parser
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = if isAlpha c
    then satisfy (\x -> x == c || x == toOppositeCase c)
    else char c
  where
    toOppositeCase :: Char -> Char
    toOppositeCase ch
        | isAlpha ch && ch >= 'a' && ch <= 'z' = toEnum (fromEnum ch - 32)
        | isAlpha ch && ch >= 'A' && ch <= 'Z' = toEnum (fromEnum ch + 32)
        | otherwise = ch 