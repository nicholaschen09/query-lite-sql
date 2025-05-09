{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueryLiteSQL.Parser.Executor where

import Data.Aeson (Value(..), Object, Array, (.:?), (.:))
import Data.Text (Text)
import Data.Vector (Vector, (!), length)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import QueryLiteSQL.Parser.SQL (SQLQuery(..), WhereClause(..), Condition(..), Op(..))

executeQuery :: Value -> SQLQuery -> Either String Value
executeQuery jsonData query = do
    -- Extract array from JSON
    array <- case jsonData of
        Array arr -> Right arr
        _ -> Left "Input must be a JSON array"

    -- Apply WHERE clause
    filteredArray <- case whereClause query of
        Nothing -> Right array
        Just whereClause' -> filterArray array (conditions whereClause')

    -- Apply column selection
    selectedArray <- selectColumns filteredArray (selectCols query)

    -- Apply LIMIT
    let limitedArray = case limitClause query of
            Nothing -> selectedArray
            Just limit' -> V.take limit' selectedArray

    return $ Array limitedArray

filterArray :: Array -> Condition -> Either String Array
filterArray array condition = do
    filtered <- V.filterM (evaluateCondition condition) array
    return filtered

evaluateCondition :: Condition -> Value -> Either String Bool
evaluateCondition condition value = case condition of
    And c1 c2 -> do
        b1 <- evaluateCondition c1 value
        b2 <- evaluateCondition c2 value
        return $ b1 && b2
    Or c1 c2 -> do
        b1 <- evaluateCondition c1 value
        b2 <- evaluateCondition c2 value
        return $ b1 || b2
    Parens c -> evaluateCondition c value
    BinaryOp col op val -> do
        obj <- case value of
            Object o -> Right o
            _ -> Left "Expected object in array"
        colVal <- case HM.lookup col obj of
            Just v -> Right v
            Nothing -> Left $ "Column not found: " ++ show col
        compareValues colVal op val

compareValues :: Value -> Op -> Value -> Either String Bool
compareValues v1 op v2 = case (v1, v2) of
    (Number n1, Number n2) -> case op of
        Eq -> Right $ n1 == n2
        Neq -> Right $ n1 /= n2
        Lt -> Right $ n1 < n2
        Gt -> Right $ n1 > n2
    (String s1, String s2) -> case op of
        Eq -> Right $ s1 == s2
        Neq -> Right $ s1 /= s2
        _ -> Left "Cannot compare strings with < or >"
    _ -> Left "Type mismatch in comparison"

selectColumns :: Array -> [Text] -> Either String Array
selectColumns array columns = do
    let selectAll = "*" `elem` columns
    if selectAll
        then return array
        else do
            selected <- V.mapM (selectColumnsFromObject columns) array
            return selected

selectColumnsFromObject :: [Text] -> Value -> Either String Value
selectColumnsFromObject columns value = case value of
    Object obj -> do
        let selected = HM.filterWithKey (\k _ -> k `elem` columns) obj
        if HM.null selected
            then Left $ "No columns found: " ++ show columns
            else return $ Object selected
    _ -> Left "Expected object in array" 