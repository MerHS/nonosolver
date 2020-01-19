module Main where

import           Control.Monad
import           Data.Char
import           Data.List
import           System.Environment
import           System.IO

data HintState =
  Hint { size   :: Int
       , pivot  :: Int -- -1 if unpivoted
       , solved :: Bool
       } deriving (Show)

data LaneState =
  Lane { hints    :: [HintState]
       , modified :: Bool
       } deriving (Show)

data BoardState =
  Board { height  :: Int
        , width   :: Int
        , dots    :: [[Bool]]
        , rowHint :: [LaneState]
        , colHint :: [LaneState]
        } deriving (Show)

------------ READER ------------

makeBlank :: Int -> Int -> [[Bool]]
makeBlank h w = replicate h (replicate w False)

sizeToHint :: Int -> HintState
sizeToHint s = Hint s (-1) False

readLane :: String -> LaneState
readLane lane =
  Lane (map (sizeToHint . read) $ words lane) False

readBoard :: String -> IO BoardState
readBoard fn = do
  handle <- openFile fn ReadMode
  whLine <- hGetLine handle
  let wh = map read $ words whLine
      w = head wh
      h = head $ tail wh
  ctn <- hGetContents handle
  let lanes = map readLane $ lines ctn
      (rh, ch) = splitAt w lanes
  hClose handle
  return $ Board h w (makeBlank h w) rh ch

putRow :: [Bool] -> IO ()
putRow row = do
  let squares = [ if c then '■' else '□' | c <- row ]
  putStrLn squares

putSolve :: (String, BoardState) -> IO ()
putSolve (fn, board) = do
  putStrLn $ "board file name: " ++ fn
  mapM_ putRow $ dots board

------------ SOLVER ------------



solveBoard :: BoardState -> BoardState
solveBoard = id

------------ CONTROL ------------


main :: IO ()
main = do
  args <- getArgs
  boards <- mapM readBoard args
  let answers = map solveBoard boards
  mapM_ putSolve (zip args answers)
