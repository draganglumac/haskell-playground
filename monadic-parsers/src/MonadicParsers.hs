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
