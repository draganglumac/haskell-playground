module ArithmeticExpressions where

import Control.Applicative
import MonadicParsers

-- expr = term + expr | term - expr | term
-- term = factor * term | factor / term | factor
-- factor = (expr) | int
-- int = ... | -2 | -1 | 0 | 1 | 2 | ...

expr :: Parser (Maybe Int)
expr = do t <- term
          (do symbol "+"
              e <- expr
              return ((+) <$> t <*> e))
           <|> (do symbol "-"
                   e <- expr
                   return ((-) <$> t <*> e))
           <|> return t

term :: Parser (Maybe Int)
term = do f <- factor
          (do symbol "*"
              t <- term
              return ((*) <$> f <*> t))
           <|> (do symbol "/"
                   t <- term
                   return (safediv f t))
           <|> return f

safediv :: Maybe Int -> Maybe Int -> Maybe Int
safediv x y = case (x, y) of
                (Just a, Just b) -> if b == 0 then Nothing else Just (div a b)
                otherwise        -> Nothing

factor :: Parser (Maybe Int)
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> Just <$> integer
