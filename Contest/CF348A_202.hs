import Control.Monad
import Data.Int

main = do
  n <- readLn :: IO Int64
  a <- (map (read :: String->Int64) . words) `liftM` getLine
  print $ max (div ((sum a) + (n-2)) (n-1) ) (maximum a)