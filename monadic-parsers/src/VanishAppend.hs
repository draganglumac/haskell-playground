module VanishAppend where

{-
PROPERTY
reverse' xs ys = reverse xs ++ ys

INDUCTIVE REASONING
reverse' [] ys = reverse [] ++ ys
               = [] ++ ys -- definition of reverse
               = ys -- definition of ++

reverse' (x:xs) ++ ys = reverse (x:xs) ++ ys
                      = (reverse xs ++ [x]) ++ ys -- definition of reverse
                      = reverse xs ++ ([x] ++ ys) -- associativity of ++
                      = reverse' xs ([x] ++ ys) -- induction hypothesis
                      = reverse' xs (x:ys) -- applying ++
-}
reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

reverse :: [a] -> [a]
reverse xs = reverse' xs []
