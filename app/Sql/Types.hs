module Sql.Types where

data Select = Select Columns From Joins (Maybe Where)
    deriving (Show)

type Database = [(Table, [(Column, ColumnType)])]

type Query = String
type Column = String
type Columns = [Column]
type Table = String
type From = Table
type Join = (Table, Pred)
type Joins = [Join]
type Where = Pred

data ColumnType
    = Int
    | Tinyint
    | Varchar Int
    | Datetime
    | Decimal Int Int
    deriving (Eq, Show)

data Pred
    = And Pred Pred
    | BinOp Op Column Column
    deriving (Eq, Show)

data Op
    = Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    deriving (Eq, Show)
