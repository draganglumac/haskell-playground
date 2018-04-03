module ArithmeticExpressions where

import Control.Applicative
import MonadicParsers

-- expr = term + expr | term
-- term = factor * term | factor
-- factor = (expr) | nat
-- nat = 0 | 1 | 2 | ...

expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e))
           <|> return t

term :: Parser Int
term = do f <- factor
          (do symbol "*"
              t <- term
              return (f * t))
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])]  -> n
            [(_, out)] -> error ("Unused input " ++ out)
            []         -> error "Invalid input"
