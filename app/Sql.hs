module Sql where

allTable :: Database
allTable = []

data Select = Select Column [From] [Join] Where

type Database = [(Table, [(Column, ColumnType)])]

type Column = String

type From = Table

type Table = String

data Join = Join Table {- ON -} Pred

type Where = Pred

data ColumnType
    = Int
    | Tinyint
    | Varchar Int
    | Datetime
    | Decimal Int Int

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
