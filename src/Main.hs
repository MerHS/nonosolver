module Main where

import           Control.Monad
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

data DotState = On | Off | Undef
instance Show DotState where
  show dot = case dot of
    On    -> "■"
    Off   -> "□"
    Undef -> "X"

data BoardState =
  Board { height  :: Int
        , width   :: Int
        , dots    :: [[DotState]]
        , rowHint :: [LaneState]
        , colHint :: [LaneState]
        } deriving (Show)

------------ READER ------------

makeBlank :: Int -> Int -> [[DotState]]
makeBlank h w = replicate h (replicate w Undef)

sizeToHint :: Int -> HintState
sizeToHint s = Hint s (-1) False

readLane :: String -> LaneState
readLane lane =
  Lane (map (sizeToHint . read) $ words lane) True

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

putRow :: [DotState] -> IO ()
putRow row = do
  let squares = concat . map show $ row
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
