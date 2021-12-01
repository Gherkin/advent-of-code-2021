import System.IO    

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
    withFile "test" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let list = fmap parseInt (lines contents)
        putStrLn $ show $ parse list)  

