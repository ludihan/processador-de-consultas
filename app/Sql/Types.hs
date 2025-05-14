module Sql.Types where

data Select = Select Columns From Joins (Maybe Where)
    deriving (Eq, Show)

type Database = [(Table, [Column])]

type Query = String
type Column = String
type Columns = [Column]
type Table = String
type From = Table
type Join = (Table, Pred)
type Joins = [Join]
type Where = [Pred]
type Pred = (Column, Op, RightPredValue)

data RightPredValue
    = RightPredLiteral String
    | RightPredColumn String
    deriving (Eq, Show)

data ColumnType
    = Int
    | Tinyint
    | Varchar Int
    | Datetime
    | Decimal Int Int
    deriving (Eq, Show)

data Op
    = Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    deriving (Eq)

instance Show Op where
    show Eq = "="
    show Ne = "<>"
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="
