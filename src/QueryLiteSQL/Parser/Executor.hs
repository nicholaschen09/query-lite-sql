{-# LANGUAGE OverloadedStrings #-}

module QueryLiteSQL.Parser.Executor
    ( executeQuery
    ) where

import Data.Aeson (Value(..), Object, Array, toJSON, fromJSON, Result(..), (.=), object)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Scientific (Scientific, toRealFloat)
import Data.Maybe (mapMaybe)

import QueryLiteSQL.Parser.SQL

-- Execute an SQL query against JSON data
executeQuery :: SQLQuery -> [Value] -> Either String Value
executeQuery query jsonData =
    case jsonData of
        [] -> Left "Empty JSON data"
        (Array arr):_ -> 
            if V.null arr
                then Left "Empty JSON array"
                else Right $ toJSON $ applyLimit query $ applyWhere query $ applySelect query (V.toList arr)
        _ -> Left "JSON data must be an array"

-- Apply SELECT clause to filter columns
applySelect :: SQLQuery -> [Value] -> [Value]
applySelect query rows =
    case sqlSelect query of
        SQLSelectAll -> rows
        SQLSelectColumns columns -> map (selectColumns columns) rows

-- Select specific columns from a JSON object
selectColumns :: [Text] -> Value -> Value
selectColumns columns (Object obj) =
    let selectedFields = mapMaybe (\col -> (col,) <$> HM.lookup col obj) columns
    in object $ map (\(k, v) -> k .= v) selectedFields
selectColumns _ val = val

-- Apply WHERE clause to filter rows
applyWhere :: SQLQuery -> [Value] -> [Value]
applyWhere query rows =
    case sqlWhere query of
        NoWhere -> rows
        SQLWhere condition -> filter (evaluateCondition condition) rows

-- Evaluate a condition for a row
evaluateCondition :: Condition -> Value -> Bool
evaluateCondition (BinaryCondition column op val) row =
    case extractValue column row of
        Just rowVal -> evaluateBinaryOp op val rowVal
        Nothing -> False
evaluateCondition (AndCondition c1 c2) row =
    evaluateCondition c1 row && evaluateCondition c2 row
evaluateCondition (OrCondition c1 c2) row =
    evaluateCondition c1 row || evaluateCondition c2 row
evaluateCondition (Parenthesized c) row =
    evaluateCondition c row

-- Evaluate a binary operation
evaluateBinaryOp :: BinaryOp -> Value -> Value -> Bool
evaluateBinaryOp Equals (StringVal s) (String t) = s == t
evaluateBinaryOp Equals (NumberVal n) (Number m) = n == (toRealFloat m :: Double)
evaluateBinaryOp Equals (ColumnRef colRef) row = 
    case extractValue colRef row of
        Just val -> evaluateBinaryOp Equals val row
        Nothing -> False

evaluateBinaryOp NotEquals val1 val2 = not (evaluateBinaryOp Equals val1 val2)

evaluateBinaryOp LessThan (NumberVal n) (Number m) = n < (toRealFloat m :: Double)
evaluateBinaryOp LessThan (ColumnRef colRef) row =
    case extractValue colRef row of
        Just val -> evaluateBinaryOp LessThan val row
        Nothing -> False
evaluateBinaryOp LessThan _ _ = False

evaluateBinaryOp GreaterThan (NumberVal n) (Number m) = n > (toRealFloat m :: Double)
evaluateBinaryOp GreaterThan (ColumnRef colRef) row =
    case extractValue colRef row of
        Just val -> evaluateBinaryOp GreaterThan val row
        Nothing -> False
evaluateBinaryOp GreaterThan _ _ = False

-- Extract a value from a JSON object by column name
extractValue :: Text -> Value -> Maybe Value
extractValue column (Object obj) = HM.lookup column obj
extractValue _ _ = Nothing

-- Apply LIMIT clause
applyLimit :: SQLQuery -> [Value] -> [Value]
applyLimit query rows =
    case sqlLimit query of
        Just n -> take n rows
        Nothing -> rows 