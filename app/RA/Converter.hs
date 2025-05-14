module RA.Converter where

import Data.List
import Database.Schema
import RA.Types
import Sql.Parser (parseSql)
import qualified Sql.Types as SqlT
import Sql.Validator (validateSqlSelect)
import Text.Megaparsec (errorBundlePretty)

type ParsingErrors = String

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
