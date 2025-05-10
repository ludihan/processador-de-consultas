module Sql.Parser where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Sql.Types as T

type Parser = Parsec Void String

test p cs = do
    parseTest p cs

parseColumn :: Parser T.Column
parseColumn =
    do
        try
            ( do
                space
                tableName <- some letterChar
                space
                _ <- single '.'
                space
                columnName <- some letterChar
                space
                pure (tableName ++ "." ++ columnName)
            )
        <|> (do
                space
                some letterChar
                space
        )

parseSelect :: Parser ()
parseSelect = do
    space
    _ <- string' "select"
    space
    _ <- single ';'
    space
    eof
    return ()

verifySelect :: T.Select -> Either String ()
verifySelect select = Right ()
