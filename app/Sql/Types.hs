module Sql.Types where

data Select = Select Column [From] [Join] Where

type Database = [(Table, [(Column, ColumnType)])]

type Query = String
type Column = String
type Table = String

type From = Table

data Join = Join Table {- ON -} Pred

type Where = Pred

data ColumnType
    = Int
    | Tinyint
    | Varchar Int
    | Datetime
    | Decimal Int Int
    deriving Show

data Pred
    = And Pred Pred
    | Or Pred Pred
    | BinOp Op Column Column

data Op
    = Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
