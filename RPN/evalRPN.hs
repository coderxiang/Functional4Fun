isNum :: String -> Bool
isNum s = s /= "*" && s /= "+" && s /= "-"

eval :: (Num a) => String -> a -> a -> a
eval op x y
  | op == "*" = x * y
  | op == "+" = x + y
  | op == "-" = y - x

evalPair :: [Double] -> String -> [Double]
evalPair l op = 
  if isNum op then ((read :: String->Double) op) : l
  else 
    case l of
      x:y:ys -> (eval op x y) : ys
      [] -> []
    
                               
evalRPN ::  String -> [Double]
evalRPN = foldl evalPair [] . words