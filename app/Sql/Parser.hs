module Sql.Parser (
    parseSql,
) where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Functor
import Data.Functor.Identity
import Sql.Types
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- keywords :: [String]
-- keywords = ["select", "from", "join", "where", "on"]

identifier :: Parser String
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')

parseStringLiteral :: Parser RightPredValue
parseStringLiteral = do
    space
    s1 <- single '\''
    s2 <- many (letterChar <|> single '@' <|> single '_' <|> single '.')
    s3 <- single '\''
    space
    return $ RightPredLiteral $ [s1] ++ s2 ++ [s3]

parseIntLiteral :: Parser RightPredValue
parseIntLiteral = do
    x <- L.signed space L.decimal
    return $ RightPredLiteral $ show (x :: Int)

parseFloatLiteral :: Parser RightPredValue
parseFloatLiteral = do
    x <- L.signed space L.float
    return $ RightPredLiteral $ show (x :: Double)

literal :: Parser RightPredValue
literal =
    try parseFloatLiteral
        <|> try parseIntLiteral
        <|> try parseStringLiteral
        <|> (parseColumn Data.Functor.<&> RightPredColumn)

operator :: Parser Op
operator =
    choice
        [ string (show Ne) >> return Ne
        , string (show Ge) >> return Ge
        , string (show Le) >> return Le
        , string (show Lt) >> return Lt
        , string (show Gt) >> return Gt
        , string (show Eq) >> return Eq
        ]

parseFrom :: Parser From
parseFrom = do
    space
    _ <- string' "from"
    space
    x <- identifier
    space
    return x

parsePred :: Parser Where
parsePred =
    let
        p :: Parser (Column, Op, RightPredValue)
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

parseJoin :: Parser Join
parseJoin =
    let
        p = do
            space
            c1 <- parseColumn
            space
            op <- operator
            space
            c2 <- parseColumn Data.Functor.<&> RightPredColumn
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

parseWhere :: Parser Where
parseWhere = do
    space
    _ <- string' "where"
    space
    predic <- parsePred
    space
    return predic

parseColumn :: Parser Column
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

parseSelect :: ParsecT Void String Identity Select
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
    return $ Select columns from join wher

parseSql :: String -> Either (ParseErrorBundle String Void) Select
parseSql = parse parseSelect "<input>"
