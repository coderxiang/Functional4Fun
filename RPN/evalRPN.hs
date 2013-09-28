evalRPN ::  (Num a, Read a) => String -> [a]
evalRPN = foldl evalOp [] . words
          where evalOp (x:y:ys) "*" = (x*y) : ys
                evalOp (x:y:ys) "+" = (x+y) : ys
                evalOp (x:y:ys) "-" = (y-x) : ys
                evalOp xs number = read number : xs