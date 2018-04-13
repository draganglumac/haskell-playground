module ExpressionStack where

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
          deriving Show

exec :: Code -> Stack -> Stack
exec [] s           = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (m + n : s)
