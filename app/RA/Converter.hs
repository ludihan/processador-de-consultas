module RA.Converter where

import RA.Types
import qualified Sql.Types as SqlT

sqlSelect2RA :: SqlT.Select -> RAExpr
sqlSelect2RA (SqlT.Select cols from joins mWhere) =
    let
        fromExpr = Value from

        joinExpr = foldl applyJoin fromExpr joins

        withSelection = case mWhere of
            Just preds -> Selection preds joinExpr
            Nothing    -> joinExpr

        projected = Projection cols withSelection
    in
        projected

applyJoin :: RAExpr -> SqlT.Join -> RAExpr
applyJoin expr (rightTable, joinPred) =
    Join [joinPred] (Value rightTable) expr
