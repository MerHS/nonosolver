module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Environment
import           System.IO

data HintState =
  Hint { size   :: Int
       , pivot  :: Int -- 0-indexed pivoted cell position-1 if unpivoted
       , solved :: Bool
       } deriving (Show)

data LaneState =
  Lane { hints    :: [HintState]
       , modified :: Bool
       , finished :: Bool
       } deriving (Show)

data CellState = On | Off | Undef deriving (Eq)
instance Show CellState where
  show cell = case cell of
    On    -> "■"
    Off   -> "□"
    Undef -> "X"

data BoardState =
  Board { height  :: Int
        , width   :: Int
        , cells   :: [[CellState]]
        , rowHint :: [LaneState]
        , colHint :: [LaneState]
        } deriving (Show)

------------ READER ------------

makeBlank :: Int -> Int -> [[CellState]]
makeBlank h w = replicate h (replicate w Undef)

sizeToHint :: Int -> HintState
sizeToHint s = Hint s (-1) False

readLane :: String -> LaneState
readLane lane =
  Lane (map (sizeToHint . read) $ words lane) True False

readBoard :: Handle -> IO BoardState
readBoard handle = do
  whLine <- hGetLine handle
  let wh = map read $ words whLine
      w = head wh
      h = head $ tail wh
  ctn <- hGetContents handle
  let lanes = map readLane $ lines ctn
      (rh, ch) = splitAt w lanes
  return $ Board h w (makeBlank h w) rh ch

putRow :: [CellState] -> IO ()
putRow row = do
  let squares = concat . map show $ row
  putStrLn squares


------------ TACTICS ------------


-- slice By Off, returns On -> True | Undef -> False List and segment left-position (zero-starting)
segments :: [CellState] -> [(Int, [Bool])]
segments lane =
  let segList = wordsBy (== Off) lane
      segState = map (map (== On)) segList
      isOffs = False:(map (/= Off) lane)
      trans = zip3 isOffs (drop 1 isOffs) [0..]
      transf = mapMaybe (\(h, t, i) -> if (not h) && t then Just i else Nothing) trans
  in
  zip transf segState

-- Step 1. by On cell, set pivots for each state
-- checkPivots :: [CellState] -> LaneState -> LaneState
-- turnOffBlank :: [CellState] -> LaneState -> [CellState]

solveLane :: LaneState -> [CellState] -> (LaneState, [CellState])
solveLane laneHint lane =
  (Lane (hints laneHint) False (finished laneHint), lane)

-- assume that lanestate are row hints.
-- transpose cellstates if you want to set column hints
setModified :: [LaneState] -> [[CellState]] -> [[CellState]] -> [LaneState]
setModified =
  zipWith3 $ \lane old new -> if old == new then lane else Lane (hints lane) True (finished lane)

solveByOverlap :: BoardState -> BoardState
solveByOverlap board =
  let oldCells = cells board
      rowSolved = zipWith solveLane (rowHint board) oldCells
      (rtemphint, rcells) = unzip rowSolved
      flipcells = transpose rcells
      ctemphint = setModified (colHint board) (transpose oldCells) flipcells
      colSolved = zipWith solveLane ctemphint flipcells
      (chint, ccells) = unzip colSolved
      newcells = transpose ccells
      rhint = setModified rtemphint rcells newcells
  in
  Board (height board) (width board) newcells rhint chint


solveByDFS :: BoardState -> BoardState
solveByDFS board = board

------------ SOLVER ------------

checkModified :: BoardState -> Bool
checkModified board =
  or (map modified $ rowHint board) || or (map modified $ colHint board)

checkSolved :: BoardState -> Bool
checkSolved board = all (notElem Undef) (cells board)

solveBoard :: BoardState -> BoardState
solveBoard board =
  let
    nextBoard = solveByOverlap board
  in
  if checkSolved nextBoard then nextBoard
  else if checkModified nextBoard then solveBoard board
  else solveByDFS nextBoard

------------ CONTROL ------------

putSolve :: BoardState -> IO ()
putSolve board = do
  mapM_ putRow $ cells board

main :: IO ()
main = do
  args <- getArgs
  mapM_
    (\fn -> withFile fn ReadMode
       (\handle -> do
          board <- readBoard handle
          putSolve $ solveBoard board))
    args
