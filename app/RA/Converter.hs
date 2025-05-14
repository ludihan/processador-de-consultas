module RA.Converter where

import Data.List
import qualified Data.Maybe
import Database.Schema
import RA.Types
import Sql.Parser
import qualified Sql.Types as SqlT
import Sql.Validator (validateSqlSelect)
import Text.Megaparsec (errorBundlePretty)

type ParsingErrors = String

sqlSelect2ra :: SqlT.Select -> RAExpr
sqlSelect2ra (SqlT.Select cols from joins mWhere) =
    let
        fromExpr = Value from

        joinExpr = foldl applyJoin fromExpr joins

        withSelection = case mWhere of
            Just preds -> Selection preds joinExpr
            Nothing -> joinExpr

        projected = Projection cols withSelection
     in
        snd $ optRemoveAttributes $ optRemoveTuples projected

optRemoveTuples :: RAExpr -> RAExpr
optRemoveTuples ra =
    let
        makeOpt _ (Projection cs v) = Projection cs (makeOpt [] v)
        makeOpt _ (Selection preds v) = makeOpt preds v
        makeOpt passingP (Join preds v1 v2) =
            Join
                preds
                (makeOpt passingP v1)
                (makeOpt passingP v2)
        makeOpt x (Value v) =
            let matches = filter (\(t, _, _) -> isColFromTable t v) x
             in if null matches
                    then
                        Value v
                    else
                        Selection matches (Value v)
     in
        makeOpt [] ra

optRemoveAttributes ra =
    let
        makeOpt _ (Projection cs v) =
            let (notin, tree) = makeOpt cs v
             in ([], Projection notin tree)
        makeOpt ats (Selection preds v) =
            let (notin, tree) = makeOpt ats v
             in (notin, Selection preds tree)
        makeOpt ats (Value v) =
            let
                cols = filter (\x -> isColFromTable x v) ats
                nottt = filter (\x -> not $ isColFromTable x v) ats
             in
                if null cols
                    then
                        (nottt, Value v)
                    else
                        (nottt, Projection cols (Value v))
        makeOpt ats (Join preds v1 v2) =
            let
                (notin1, tree1) = makeOpt ats v1
                (notin2, tree2) = makeOpt ats v2
             in
                ( notin1 ++ notin2
                , Join
                    preds
                    tree1
                    tree2
                )
     in
        makeOpt [] ra

applyJoin :: RAExpr -> SqlT.Join -> RAExpr
applyJoin expr (table, joinPred) =
    Join [joinPred] (Value table) expr

raExpr2QueryPlan :: RAExpr -> [String]
raExpr2QueryPlan (Value rel) =
    [show (Value rel)]
raExpr2QueryPlan (Projection attr expr) =
    ("π " ++ intercalate ", " attr) : raExpr2QueryPlan expr
raExpr2QueryPlan (Selection p expr) =
    ("σ " ++ prettyPreds p) : raExpr2QueryPlan expr
raExpr2QueryPlan (Join p expr1 expr2) =
    ("|X| " ++ prettyPreds p) : raExpr2QueryPlan expr1 ++ raExpr2QueryPlan expr2

sqlSelectString2ra :: String -> Either [ParsingErrors] RAExpr
sqlSelectString2ra sel =
    case parseSql sel of
        Left bundle -> Left $ Prelude.lines (errorBundlePretty bundle)
        Right parsed ->
            case validateSqlSelect parsed of
                [] -> Right (sqlSelect2ra parsed)
                xs -> Left xs

sqlSelectString2QueryPlan :: String -> Either [ParsingErrors] [String]
sqlSelectString2QueryPlan sel = do
    x <- sqlSelectString2ra sel
    pure $ raExpr2QueryPlan x

filterColsFromTable :: Attributes -> Relation -> Attributes
filterColsFromTable cols table =
    let colsFromTable = concatMap snd $ filter (\x -> table == fst x) lowercaseDatabase
        filtered = filter (`elem` colsFromTable) cols
     in filtered
