module Sql.Parser where

import Data.Char

parse :: Parser a -> String -> Either String (a, ParserState)
parse parser cs = runParser parser $ ParserState cs (Position 1 1)

advancePosition :: String -> Position -> Position
advancePosition cs pos =
    foldl
        ( \pos' c ->
            if c == '\n'
                then
                    Position (succ (line pos')) 1
                else
                    Position (line pos') (succ (column pos'))
        )
        pos
        cs

newtype Parser a = Parser
    { runParser :: ParserState -> Either String (a, ParserState)
    }

data ParserState = ParserState
    { input :: String
    , position :: Position
    }
    deriving (Show)

data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving (Show)

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $
        \s -> do
            (x, rest) <- p s
            Right (f x, rest)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $
        \s ->
            Right (x, s)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser $
        \s -> do
            (f, rest) <- p1 s
            (x, rest') <- p2 rest
            pure (f x, rest')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= f = Parser $
        \s -> do
            (a, rest) <- p s
            runParser (f a) rest

item :: Parser Char
item = Parser $
    \s -> case input s of
        [] ->
            Left $
                concat
                    [ "didnt find any characters in line "
                    , show (line (position s))
                    , " at column "
                    , show (column (position s))
                    ]
        c : rest ->
            Right
                ( c
                , ParserState rest (advancePosition [c] (position s))
                )

char :: Char -> Parser Char
char c = Parser $
    \s -> do
        (c1, state1) <- runParser item s
        if c1 == c
            then
                Right (c1, state1)
            else
                Left "Couldnt :/"

whitespace :: Parser ()
whitespace = Parser $
    \s ->
        case input s of
            [] ->
                Right ((), s)
            (c : cs) ->
                if isSpace c
                    then
                        runParser whitespace s{input = cs}
                    else
                        Right ((), s)

parseSelect :: Parser String
parseSelect = do
    y <- item
    x <- item
    return [y, x]
