import System.IO
import Text.Read

data Move = Forward Int | Up Int | Down Int deriving (Show)
data Pos = Pos Int Int deriving (Show)


compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

parseInt :: String -> Maybe Int
parseInt x = readMaybe x :: Maybe Int

splitString' :: String -> String -> Char -> Maybe (String, String)
splitString' _ [] _ = Nothing
splitString' x (y:ys) c = 
  if y == c
  then Just (x, ys)
  else splitString' (x ++ [y]) ys c

splitString :: String -> Char -> Maybe (String, String)
splitString x c = splitString' [] x c

parseMove' :: (String, String) -> Maybe Move
parseMove' ("forward", x) = do
  steps <- parseInt x
  Just $ Forward steps
parseMove' ("up", x) = do
  steps <- parseInt x
  Just $ Up steps
parseMove' ("down", x) = do
  steps <- parseInt x
  Just $ Down steps

parseMove :: String -> Maybe Move
parseMove x = splitString x ' ' >>= parseMove'

doMove :: Move -> Pos -> Pos
doMove (Forward steps) (Pos x y) = Pos (x + steps) y
doMove (Up steps) (Pos x y) = Pos x (y + steps)
doMove (Down steps) (Pos x y) = Pos x (y - steps)



main = do     
    withFile "part1.test" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let list = lines contents
        let moves = fmap parseMove list
        fs <- sequence $ fmap (\x -> fmap doMove x) moves
        putStrLn $ show $ compose fs)  
