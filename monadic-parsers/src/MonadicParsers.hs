{-# LANGUAGE InstanceSigs #-}

module MonadicParsers where

import Control.Applicative
import Data.Char

-- empty list denotes failure, singleton list denotes success
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                    [] -> []
                    (x:xs) -> [(x, xs)])

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input -> case parse p input of
                          [] -> []
                          [(v, output)] -> [(g v, output)])

instance Applicative Parser where
  --pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])

  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                         [] -> []
                         [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                       [] -> []
                       [(v, out)] -> parse (f v) out)
