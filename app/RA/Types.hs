module RA.Types where

import Data.List
import qualified Sql.Types as SqlT

type Relation = SqlT.Table
type Relations = [Relation]
type Attribute = SqlT.Column
type Attributes = [Attribute]

data RAExpr
    = Value Relation
    | Projection [Attribute] RAExpr
    | Selection Pred RAExpr
    | Cross RAExpr RAExpr
    | Join Pred RAExpr RAExpr

instance Show RAExpr where
    show = go 0
      where
        go :: Int -> RAExpr -> String
        go indent ra =
            let
                indentLine :: String -> String
                indentLine s = replicate indent ' ' ++ s

                prettyPreds :: [(SqlT.Column, SqlT.Op, SqlT.RightPredValue)] -> String
                prettyPreds = intercalate ", " . map prettyPred

                prettyPred :: (SqlT.Column, SqlT.Op, SqlT.RightPredValue) -> String
                prettyPred (col, op, val) = col ++ " " ++ prettyOp op ++ " " ++ prettyVal val

                prettyOp :: SqlT.Op -> String
                prettyOp SqlT.Eq = "="
                prettyOp SqlT.Ne = "!="
                prettyOp SqlT.Lt = "<"
                prettyOp SqlT.Le = "<="
                prettyOp SqlT.Gt = ">"
                prettyOp SqlT.Ge = ">="

                prettyVal :: SqlT.RightPredValue -> String
                prettyVal (SqlT.RightPredLiteral s) = "'" ++ s ++ "'"
                prettyVal (SqlT.RightPredColumn c) = c

                ind = indent + 2
             in
                case ra of
                    Value rel ->
                        indentLine $ "Relation: " ++ rel
                    Projection attrs sub ->
                        indentLine ("Projection: " ++ intercalate ", " attrs) ++ "\n" ++ go ind sub
                    Selection preds sub ->
                        indentLine ("Selection: " ++ prettyPreds preds) ++ "\n" ++ go ind sub
                    Join preds l r ->
                        indentLine ("Join: " ++ prettyPreds preds) ++ "\n" ++ go ind l ++ "\n" ++ go ind r
                    Cross l r ->
                        indentLine "Cross" ++ "\n" ++ go ind l ++ "\n" ++ go ind r
type Value = String

type RightPredValue = SqlT.RightPredValue
type Pred = [(Attribute, SqlT.Op, RightPredValue)]
