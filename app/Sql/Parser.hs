module Sql.Parser where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Sql.Types as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Functor

type Parser = Parsec Void String

keywords :: [String]
keywords = ["select", "from", "join", "where", "on"]

identifier :: Parser String
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')

parseStringLiteral :: Parser T.RightPredValue
parseStringLiteral = do
    space
    s1 <- single '\''
    s2 <- many (letterChar <|> single '@' <|> single '_' <|> single '.')
    s3 <- single '\''
    space
    return $ T.RightPredLiteral $ [s1] ++ s2 ++ [s3]

parseIntLiteral :: Parser T.RightPredValue
parseIntLiteral = do
    x <- L.signed space L.decimal
    return $ T.RightPredLiteral $ show (x :: Int)

parseFloatLiteral :: Parser T.RightPredValue
parseFloatLiteral = do
    x <- L.signed space L.float
    return $ T.RightPredLiteral $ show (x :: Double)

literal :: Parser T.RightPredValue
literal =
    try parseFloatLiteral
        <|> try parseIntLiteral
        <|> try parseStringLiteral
        <|> (parseColumn Data.Functor.<&> T.RightPredColumn)

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

parsePred :: Parser T.Where
parsePred =
    let
        p :: Parser (T.Column, T.Op, T.RightPredValue)
        p = do
            space
            col <- parseColumn
            space
            op <- operator
            space
            v <- literal
            space
            return (col, op, v)
     in
        do
            space
            p `sepBy1` (space >> string' "and" >> space)

parseJoin :: Parser T.Join
parseJoin =
    let
        p = do
            space
            c1 <- parseColumn
            space
            op <- operator
            space
            c2 <- parseColumn Data.Functor.<&> T.RightPredColumn
            return (c1, op, c2)
     in
        do
            space
            _ <- string' "join"
            space
            table <- identifier
            space
            _ <- string' "on"
            space
            cond <- p
            space
            return (table, cond)

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
            table <- identifier
            space
            _ <- single '.'
            space
            column <- identifier
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
    columns <-
        try
            ( parseColumn
                `sepBy1` (space >> single ',' >> space)
            )
            <|> ( do
                    space
                    x <- string' "*"
                    space
                    return [x]
                )

    space
    from <- parseFrom
    space
    join <- many parseJoin
    space
    wher <- optional parseWhere
    space
    _ <- some (single ';')
    space
    eof
    return $ T.Select columns from join wher
