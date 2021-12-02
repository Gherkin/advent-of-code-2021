import System.IO    

slideWindow :: [Int] -> [Int]
slideWindow [] = []
slideWindow (x:y:z:[]) =
  [sum [x, y, z]]
slideWindow (x:y:z:xs) =
  (sum [x, y, z]) : slideWindow (y : (z : xs))



parse :: [Int] -> Int
parse (x:y:[]) =
  if y > x
  then 1
  else 0

parse (x:y:xs) =
  if y > x
  then 1 + parse (y : xs)
  else 0 + parse (y : xs)

parseInt :: String -> Int
parseInt x = read x :: Int
 
    
main = do     
    withFile "part1.input" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let list = fmap parseInt (lines contents)
        let slid = slideWindow list
        putStrLn $ show $ parse slid)  
