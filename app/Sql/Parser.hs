module Sql.Parser where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.List (intercalate)
import qualified Sql.Types as T

type Parser = Parsec Void String

selectPretty :: String -> IO ()
selectPretty sel =
    let a = parse parseSelect "<input>" sel
        formatList = intercalate ", "
     in case a of
            Left bundle -> putStr (errorBundlePretty bundle)
            Right (T.Select cols from joins wher) ->
                putStrLn $
                    "Select\n"
                        ++ "cols: "
                        ++ formatList cols
                        ++ "\nfrom: "
                        ++ from
                        ++ "\njoin: "
                        ++ show joins
                        ++ "\nwhere: "
                        ++ show wher

keywords :: [String]
keywords = ["select", "from", "join", "where", "on"]

identifier :: Parser String
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> single '_')

literal :: Parser T.Literal
literal =
    let
        accepted = (alphaNumChar <|> single '_' <|> single '-' <|> single '@')
     in
        try (some accepted)
            <|> between (char '\'') (char '\'') (some (accepted <|> single '.'))

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

parsePred :: Parser [T.Pred]
parsePred =
    let
        p :: Parser T.Pred
        p = do
            space
            column1 <- parseColumn
            space
            op <- operator
            space
            v <- literal
            space
            return $ T.BinOp op column1 v
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
            c2 <- parseColumn
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
    columns <- parseColumn `sepBy1` (space >> single ',' >> space)
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
