module Sql.Types where

data Select = Select Columns From Joins (Maybe Where)
    deriving (Show)

type Database = [(Table, [Column])]

type Query = String
type Column = String
type Columns = [Column]
type Table = String
type From = Table
type Join = (Table, (Column, Op, Column))
type Joins = [Join]
type Where = [(Column, Op, Literal)]

type Literal = String
-- int | float | string

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
    deriving (Eq, Show)
