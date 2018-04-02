module MonadicParsers where

import Control.Applicative
import Data.Char

-- empty list denotes failure, and singleton list denotes success
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> (a, String)
parse (P p) input = p input

item :: Parser Char
item = (\case
        [] -> []
        (x:xs) -> [(x, xs)])
