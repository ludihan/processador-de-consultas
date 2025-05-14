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
    | Join Pred RAExpr RAExpr

instance Show RAExpr where
    show = prettyRAExpr 0

type Value = String

type RightPredValue = SqlT.RightPredValue
type Pred = [(Attribute, SqlT.Op, RightPredValue)]

prettyPreds :: [(SqlT.Column, SqlT.Op, SqlT.RightPredValue)] -> String
prettyPreds = intercalate ", " . map prettyPred

prettyPred :: (SqlT.Column, SqlT.Op, SqlT.RightPredValue) -> String
prettyPred (col, op, val) = col ++ " " ++ show op ++ " " ++ show val

prettyRAExpr :: Int -> RAExpr -> String
prettyRAExpr indLvl ra =
    let
        indentConcat :: [String] -> String
        indentConcat s = replicate indLvl ' ' ++ concat s

        ind = indLvl + 2
     in
        case ra of
            Value rel ->
                indentConcat
                    [ "("
                    , rel
                    , ")"
                    ]
            Projection attrs sub ->
                indentConcat
                    [ "π " ++ intercalate ", " attrs
                    , "\n"
                    , prettyRAExpr ind sub
                    ]
            Selection preds sub ->
                indentConcat
                    [ "σ " ++ prettyPreds preds
                    , "\n"
                    , prettyRAExpr ind sub
                    ]
            Join preds l r ->
                indentConcat
                    [ "|X| " ++ prettyPreds preds
                    , "\n"
                    , prettyRAExpr (ind + 2) l
                    , "\n"
                    , prettyRAExpr (ind + 2) r
                    ]
