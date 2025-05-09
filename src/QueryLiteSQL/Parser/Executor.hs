{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QueryLiteSQL.Parser.Executor where

import Data.Aeson (Value(..))
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Vector (Vector, (!), length)
import qualified Data.Vector as V
import Data.List (elem)
import QueryLiteSQL.Parser.SQL (SQLQuery(..), WhereClause(..), Condition(..), Op(..))
import Data.Aeson.Key (fromText, toText)

type QueryResult = Either String [Value]

executeQuery :: SQLQuery -> [Value] -> QueryResult
executeQuery query data_ = do
    filtered <- filterData query data_
    selected <- selectColumns query filtered
    limited <- applyLimit query selected
    return limited

filterData :: SQLQuery -> [Value] -> QueryResult
filterData query data_ = case whereClause query of
    Nothing -> Right data_
    Just whereClause' -> filterByCondition (conditions whereClause') data_

filterByCondition :: Condition -> [Value] -> QueryResult
filterByCondition condition data_ = do
    filtered <- mapM (evaluateCondition condition) data_
    return $ [v | (v, True) <- zip data_ filtered]

evaluateCondition :: Condition -> Value -> Either String Bool
evaluateCondition (And c1 c2) obj = do
    b1 <- evaluateCondition c1 obj
    b2 <- evaluateCondition c2 obj
    return $ b1 && b2
evaluateCondition (Or c1 c2) obj = do
    b1 <- evaluateCondition c1 obj
    b2 <- evaluateCondition c2 obj
    return $ b1 || b2
evaluateCondition (BinaryOp col op val) obj = case obj of
    Object objMap -> do
        colVal <- case KM.lookup (fromText col) objMap of
            Just v -> Right v
            Nothing -> Left $ "Column not found: " ++ show col
        compareValues op colVal val
    _ -> Left "Expected object value"
evaluateCondition (Parens c) obj = evaluateCondition c obj

compareValues :: Op -> Value -> Value -> Either String Bool
compareValues op v1 v2 = case (v1, v2) of
    (Number n1, Number n2) -> Right $ case op of
        Eq -> n1 == n2
        Neq -> n1 /= n2
        Lt -> n1 < n2
        Gt -> n1 > n2
    (String s1, String s2) -> Right $ case op of
        Eq -> s1 == s2
        Neq -> s1 /= s2
        Lt -> s1 < s2
        Gt -> s1 > s2
    (Bool b1, Bool b2) -> Right $ case op of
        Eq -> b1 == b2
        Neq -> b1 /= b2
        _ -> False
    _ -> Left "Type mismatch in comparison"

selectColumns :: SQLQuery -> [Value] -> QueryResult
selectColumns query data_ = do
    selected <- mapM (selectRow $ selectCols query) data_
    return selected

selectRow :: [Text] -> Value -> Either String Value
selectRow columns (Object objMap) = do
    let selected = KM.filterWithKey (\k _ -> toText k `elem` columns) objMap
    if KM.null selected
        then Left "No matching columns found"
        else return $ Object selected
selectRow _ _ = Left "Expected object value"

applyLimit :: SQLQuery -> [Value] -> QueryResult
applyLimit query data_ = case limitClause query of
    Nothing -> Right data_
    Just limit -> Right $ take limit data_ 