{-# LANGUAGE LambdaCase #-}

module Sql.Parser where

newtype Parser a = Parser
    { runParser :: String -> Either String (a, String)
    }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $
        \input -> do
            (x, rest) <- p input
            Right (f x, rest)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = Parser $
        \input ->
            Right (x, input)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser p1) <*> (Parser p2) = Parser $
        \input -> do
            (f, rest) <- p1 input
            (x, rest') <- p2 rest
            pure (f x, rest')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p >>= f = Parser $
        \input -> do
            (a, rest) <- p input
            runParser (f a) rest

item :: Parser Char
item = Parser $
    \case
        "" -> Left "empty"
        (c : cs') -> Right (c, cs')
