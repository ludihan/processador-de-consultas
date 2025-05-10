module Sql.Parser where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative (Alternative)
import qualified Sql.Types as T

type Parser = Parsec Void String

keywords :: [String]
keywords = ["select", "from", "join", "where", "on"]

identifier :: Parser String
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')

operator :: Parser T.Op
operator =
    choice
        [ string "<>" >> return T.Ne
        , string ">=" >> return T.Ge
        , string "<=" >> return T.Le
        , string "<" >> return T.Lt
        , string ">" >> return T.Gt
        , string "=" >> return T.Eq
        ]

parseFrom :: Parser T.From
parseFrom = do
    space
    _ <- string' "from"
    space
    x <- identifier
    space
    return x

parsePredExpr :: Parser T.Pred
parsePredExpr = do
    space
    column1 <- identifier
    space
    op <- operator
    space
    column2 <- identifier
    space
    return $ T.BinOp op column1 column2

parsePred :: Parser T.Pred
parsePred = do
    space
    preds <- parsePredExpr

    return $ T.BinOp T.Eq "a" "a"

parseJoin :: Parser T.Join
parseJoin = do
    space
    _ <- string' "join"
    space
    table <- identifier
    space
    on <- string' "on"
    space
    predic <- parsePred
    space
    return (table, predic)

parseWhere :: Parser T.Where
parseWhere = do
    space
    _ <- string' "where"
    space
    predic <- parsePred
    space
    return predic

parseColumn :: Parser T.Column
parseColumn =
    try
        ( do
            space
            table <- some letterChar
            space
            _ <- single '.'
            space
            column <- some letterChar
            space
            return (table ++ "." ++ column)
        )
        <|> ( do
                space
                column <- some letterChar
                space
                return column
            )

parseSelect :: Parser T.Select
parseSelect = do
    space
    _ <- string' "select"
    space
    columns <- parseColumn `sepBy1` (single ',' >> space)
    space
    from <- parseFrom
    space
    join <- many parseJoin
    space
    wherE <- optional parseWhere
    space
    _ <- some (single ';')
    space
    eof
    return $ T.Select columns from join wherE

validateSelect :: T.Select -> Either String ()
validateSelect stmt = Right ()
