module Sql.Parser where

parse :: Parser a -> String -> Either String (a, ParserState)
parse parser cs = runParser parser $ ParserState cs 1 1

newtype Parser a = Parser
    { runParser :: ParserState -> Either String (a, ParserState)
    }

data ParserState = ParserState
    { input :: String
    , line :: Int
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
        "" ->
            Left $
                "didnt find any characters in line "
                    ++ show (line s)
                    ++ " at column "
                    ++ show (column s)
        (c : rest) ->
            let
                line' =
                    if c == '\n' then line s + 1 else line s + 0
                column' =
                    (column s + 1)
             in
                Right
                    ( c
                    , s
                        { input = rest
                        , line = line'
                        , column = column'
                        }
                    )

parseSelect = do
    _ <- item
    item
