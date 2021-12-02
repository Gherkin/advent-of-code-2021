import System.IO
import Text.Read
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Class (lift)

data Move = Forward Int | Up Int | Down Int deriving (Show)
data Pos = Pos Int Int Int deriving (Show)


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
doMove (Forward steps) (Pos x y a) = Pos (x + steps) (y + a * steps) a
doMove (Up steps) (Pos x y a) = Pos x y (a - steps)
doMove (Down steps) (Pos x y a) = Pos x y (a + steps)


maybeToIO :: Maybe a -> IO a
maybeToIO Nothing = error "nothing!"
maybeToIO (Just x) = return x

mult :: Pos -> Int
mult (Pos x y _) = x * y

main = do     
    withFile "part1.input" ReadMode (\handle -> do  
        contents <- hGetContents handle
        let list = lines contents
        let moves = fmap parseMove list
        fs <- maybeToIO $ sequence $ fmap (\x -> fmap doMove x) moves
        let pos = compose fs (Pos 0 0 0)
        putStrLn $ show $ mult pos)  
