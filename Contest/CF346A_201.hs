gcdOfList :: [Int]->Int
gcdOfList x = case x of [] -> 0
                        (x : xs) -> gcd x (gcdOfList xs)
   
solve :: String->Int->String
solve s n = 
  if rem ( (div ma g) - n) 2 == 0 then "Bob" else "Alice"
  where a = map (read :: String->Int) (words s)
        ma = maximum a
        g = gcdOfList a
        
main = do
  n <- getLine
  s <- getLine
  putStrLn $ solve s ((read :: String->Int)  n)