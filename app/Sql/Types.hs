module Sql.Types where

data Select = Select [Column] From (Maybe [Join]) (Maybe Where)
    deriving (Show)

type Database = [(Table, [(Column, ColumnType)])]

type Query = String
type Column = String
type Table = String

type Where = Pred
type From = Table

data Join = Join Table Pred
    deriving (Show)

data ColumnType
    = Int
    | Tinyint
    | Varchar Int
    | Datetime
    | Decimal Int Int
    deriving (Eq, Show)

data Pred
    = And Pred Pred
    | Or Pred Pred
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
