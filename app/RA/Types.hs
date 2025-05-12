module RA.Types where

import qualified Sql.Types as SqlT

type Relation = SqlT.Table
type Attribute = SqlT.Column

data RAExpr
    = Value Relation
    | Projection [Attribute] RAExpr
    | Selection Pred RAExpr
    | Cross RAExpr RAExpr
    | Join Pred RAExpr RAExpr
    deriving (Show)

type Value = String

type Pred = [(Attribute, SqlT.Op, Value)]
