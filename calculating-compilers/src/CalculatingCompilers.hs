module CalculatingCompilers where

data Expr = Val Int
          | Add Expr Expr
          deriving Show

-- eval :: Expr -> Int
-- eval (Val n) = n
-- eval (Add x y) = eval x + eval y

type Stack = [Int]

-- Property => eval' e s = eval e : s
-- definition derived from inductive proof of the property
-- eval' :: Expr -> Stack -> Stack
-- eval' (Val n) s = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push = (:)

add :: Stack -> Stack
add (m : n : s) = m + n : s

-- more efficient eval function using the accumulating-style eval'
eval :: Expr -> Int
eval e = head (eval' e [])

-- the next step is to transform stack-based evaluation to
-- CONTINUATION style Stack -> Stack
type Cont = Stack -> Stack

-- property eval'' e c s = c (eval' e s)
-- and again definition derived from the inductive proof of the property
-- eval'' :: Expr -> Cont -> Cont
-- eval'' (Val n) c s = c (push n s)
-- eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s
--
-- eval' :: Expr -> Cont
-- eval' e s = eval'' e id s

-- Defunctionalising - bringing it back to first-order style
-- only 3 combinators used in definition of eval' and eval''
-- id, add, and push
haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

-- so now, rewriting eval' and eval'' in first-order style
eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n) c = pushC n c
eval'' (Add x y) c = eval'' x (eval'' y (addC c))

-- and now the ADT for machine code
data Code = HALT | PUSH Int Code | ADD Code
            deriving Show

-- exec :: Code -> Cont
-- exec HALT = haltC
-- exec (PUSH n c) = pushC n (exec c)
-- exec (ADD c) = addC (exec c)
--
-- and when we expand definitions of Code ADT we get
-- exec HALT s = haltC s
--             = id s = s
-- exec (PUSH n c) s = pushC n (exec c) s
--                   = (exec c . push n) s
--                   = exec c (push n s)
--                   = exec c (n : s)
-- exec (ADD c) s = addC (exec c) s
--                = (exec c . add) s
--                = exec c (add s)
--                = exec c (add (m : n : s'))
--                = exec c (m + n : s')
exec :: Code -> Stack -> Stack
exec HALT s              = s
exec (PUSH n c) s        = exec c (n : s)
exec (ADD c) (m : n : s) = exec c (m + n : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
