module RA.Converter where

import Database.Schema
import RA.Types
import qualified Sql.Types as SqlT

sqlSelect2ra :: SqlT.Select -> RAExpr
sqlSelect2ra (SqlT.Select cols from joins mWhere) =
    let
        -- projectionOpt = [(t,filterColsFromTable h)]
        fromExpr = Value from

        joinExpr = foldl applyJoin fromExpr joins

        withSelection = case mWhere of
            Just preds -> Selection preds joinExpr
            Nothing -> joinExpr

        projected = Projection cols withSelection
     in
        projected

applyJoin :: RAExpr -> SqlT.Join -> RAExpr
applyJoin expr (table, joinPred) =
    Join [joinPred] (Value table) expr

ra2QueryPlan :: RAExpr -> [String]
ra2QueryPlan (Value rel) = [rel]
ra2QueryPlan (Projection attr expr) = ("Projection" ++ show attr) : ra2QueryPlan expr
ra2QueryPlan (Selection p expr) = ("Selection" ++ show p) : ra2QueryPlan expr
ra2QueryPlan (Cross expr1 expr2) = "Cross" : (ra2QueryPlan expr1 ++ ra2QueryPlan expr2)
ra2QueryPlan (Join p expr1 expr2) = ("Join" ++ show p) : ra2QueryPlan expr1 ++ ra2QueryPlan expr2

filterColsFromTable :: Attributes -> Relation -> Attributes
filterColsFromTable cols table =
    let colsFromTable = concatMap snd $ filter (\x -> table == fst x) lowercaseDatabase
        filtered = filter (`elem` colsFromTable) cols
     in filtered
