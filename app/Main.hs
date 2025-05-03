module Main where

import qualified Sql

main :: IO ()
main = putStrLn "Hello, Haskell!"

tabela :: Sql.Table
tabela = "teste" :: Sql.Table
