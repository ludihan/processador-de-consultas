module RA.Converter where

import Database.Schema
import RA.Types
import qualified Sql.Types as SqlT

sqlSelect2RA :: SqlT.Select -> RAExpr
sqlSelect2RA (SqlT.Select cols from joins mWhere) =
    let
        -- projectionOpt = [(t,filterColsFromTable h)]
        fromExpr = Value from

        joinExpr = foldl applyJoin fromExpr joins

        withSelection = case mWhere of
            Just preds -> Selection preds joinExpr
            Nothing    -> joinExpr

        projected = Projection cols withSelection

    in
        projected

applyJoin :: RAExpr -> SqlT.Join -> RAExpr
applyJoin expr (table, joinPred) =
    Join [joinPred] (Value table) expr

filterColsFromTable :: Attributes -> Relation -> Attributes
filterColsFromTable cols table =
    let colsFromTable = concatMap snd $ filter (\x -> table==fst x) lowercaseDatabase
        filtered = filter (`elem` colsFromTable) cols
    in filtered
