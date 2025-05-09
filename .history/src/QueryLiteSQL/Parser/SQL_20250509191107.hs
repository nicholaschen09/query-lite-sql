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
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import Data.Functor (($>))

-- SQL Value types
data Value = StringVal Text
           | NumberVal Double
           | ColumnRef Text
           deriving (Show, Eq)

-- SQL Query structure
data SQLQuery = SQLQuery
    { sqlSelect :: SQLSelect
    , sqlWhere  :: SQLWhere
    , sqlLimit  :: Maybe Int
    } deriving (Show, Eq)

-- SQL SELECT clause
data SQLSelect = SQLSelectAll
               | SQLSelectColumns [Text]
               deriving (Show, Eq)

-- SQL WHERE clause
data SQLWhere = NoWhere
              | SQLWhere Condition
              deriving (Show, Eq)

-- SQL Condition
data Condition = BinaryCondition Text BinaryOp Value
               | AndCondition Condition Condition
               | OrCondition Condition Condition
               | Parenthesized Condition
               deriving (Show, Eq)

-- Binary operators
data BinaryOp = Equals
              | NotEquals
              | LessThan
              | GreaterThan
              deriving (Show, Eq)

-- Main parser for SQL Queries
parseSQL :: Text -> Either String SQLQuery
parseSQL input = parseOnly sqlQueryParser (T.toLower input)

-- Space and keyword helpers
skipSpace1 :: Parser ()
skipSpace1 = skipMany1 space

keyword :: Text -> Parser Text
keyword kw = do
    result <- A.takeWhile isAlphaNum
    if result == kw
        then return result
        else fail $ "Expected keyword: " ++ T.unpack kw

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = satisfy (\x -> toLower x == toLower c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString = mapM caseInsensitiveChar

sqlQueryParser :: Parser SQLQuery
sqlQueryParser = do
    skipSpace
    _ <- caseInsensitiveString "select"
    skipSpace1
    selectPart <- selectParser
    skipSpace1
    _ <- caseInsensitiveString "from"
    skipSpace1
    _ <- tableNameParser
    skipSpace
    wherePart <- option NoWhere whereParser
    skipSpace
    limitPart <- option Nothing limitParser
    skipSpace
    endOfInput
    return $ SQLQuery selectPart wherePart limitPart

-- Parse table name (we don't actually use it in this application)
tableNameParser :: Parser Text
tableNameParser = A.takeWhile isAlphaNum

-- Parser for SELECT part
selectParser :: Parser SQLSelect
selectParser = (char '*' $> SQLSelectAll)
           <|> (SQLSelectColumns <$> columnList)

-- Parse a list of columns
columnList :: Parser [Text]
columnList = columnName `sepBy1` (skipSpace *> char ',' *> skipSpace)

-- Parse a column name
columnName :: Parser Text
columnName = A.takeWhile $ \c -> isAlphaNum c || c == '_'

-- Parser for WHERE part
whereParser :: Parser SQLWhere
whereParser = do
    _ <- caseInsensitiveString "where"
    skipSpace1
    SQLWhere <$> conditionParser

-- Parse a condition
conditionParser :: Parser Condition
conditionParser = orConditionParser

-- Parse OR condition (highest precedence)
orConditionParser :: Parser Condition
orConditionParser = do
    left <- andConditionParser
    option left $ do
        skipSpace
        _ <- caseInsensitiveString "or"
        skipSpace1
        right <- orConditionParser
        return $ OrCondition left right

-- Parse AND condition (medium precedence)
andConditionParser :: Parser Condition
andConditionParser = do
    left <- binaryConditionParser
    option left $ do
        skipSpace
        _ <- caseInsensitiveString "and"
        skipSpace1
        right <- andConditionParser
        return $ AndCondition left right

-- Parse binary conditions or parenthesized expressions (lowest precedence)
binaryConditionParser :: Parser Condition
binaryConditionParser = parenthesizedCondition <|> simpleBinaryCondition

-- Parse a parenthesized condition
parenthesizedCondition :: Parser Condition
parenthesizedCondition = do
    skipSpace
    _ <- char '('
    skipSpace
    condition <- conditionParser
    skipSpace
    _ <- char ')'
    return $ Parenthesized condition

-- Parse a simple binary condition (e.g., "column = value")
simpleBinaryCondition :: Parser Condition
simpleBinaryCondition = do
    skipSpace
    col <- columnName
    skipSpace
    op <- operatorParser
    skipSpace
    val <- valueParser
    return $ BinaryCondition col op val

-- Parse a binary operator
operatorParser :: Parser BinaryOp
operatorParser = (string "=" $> Equals)
              <|> (string "!=" $> NotEquals)
              <|> (string "<" $> LessThan)
              <|> (string ">" $> GreaterThan)

-- Parse a value (string, number, or column reference)
valueParser :: Parser Value
valueParser = stringValueParser
          <|> numberValueParser
          <|> (ColumnRef <$> columnName)

-- Parse a string value (quoted)
stringValueParser :: Parser Value
stringValueParser = do
    _ <- char '\''
    content <- A.takeWhile (/= '\'')
    _ <- char '\''
    return $ StringVal content

-- Parse a number value
numberValueParser :: Parser Value
numberValueParser = do
    sign <- option "" (string "-")
    intPart <- A.takeWhile1 isDigit
    fractPart <- option "0" $ do
        _ <- char '.'
        A.takeWhile1 isDigit
    let numText = T.concat [sign, intPart, ".", fractPart]
    case reads (T.unpack numText) of
        [(num, "")] -> return $ NumberVal num
        _           -> fail "Invalid number"

-- Parser for LIMIT part
limitParser :: Parser (Maybe Int)
limitParser = do
    _ <- caseInsensitiveString "limit"
    skipSpace1
    numStr <- A.takeWhile1 isDigit
    case reads (T.unpack numStr) of
        [(num, "")] -> return $ Just num
        _           -> fail "Invalid limit number" 