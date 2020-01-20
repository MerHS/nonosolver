module NonoSolver
    (
      HintState (..),
      LaneState (..),
      CellState (..),
      BoardState (..),
      readLane,
      readBoard,
      printRow,
      printBoard,
      solveBoard,
      solveFileAndPrint
    ) where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.IO

data HintState =
  Hint { size    :: Int
       , exRange :: Maybe (Int, Int) -- expected range
       , pivot   :: Maybe (Int, Int) -- 0-indexed pivoted cell position-1 if unpivoted
       , solved  :: Bool
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
sizeToHint s = Hint s Nothing Nothing False

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


------------ TACTICS ------------

-- slice By Off, returns On -> True | Undef -> False List and segment left-position (zero-starting)
data Segment =
  Segment { leftPos :: Int
          , onState :: [Bool]
          } deriving (Show)

segments :: [CellState] -> [Segment]
segments lane =
  let segList = wordsBy (== Off) lane
      segState = map (map (== On)) segList
      isOffs = False:(map (/= Off) lane)
      trans = zip3 isOffs (drop 1 isOffs) [0..]
      transf = mapMaybe (\(h, t, i) -> if (not h) && t then Just i else Nothing) trans
  in
  zipWith Segment transf segState

-- Tactic 1. by On cell, set pivots for each state
-- checkPivots :: [CellState] -> LaneState -> LaneState

--
-- turnOffBlank :: [CellState] -> LaneState -> [CellState]

-- solve each lane
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
  -- let oldCells = cells board
  --rowSolved <- zipWithM solveLane (rowHint board) oldCells
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

printRow :: [CellState] -> IO ()
printRow row = do
  let squares = concat . map show $ row
  putStrLn squares

printBoard :: BoardState -> IO ()
printBoard board = do
  mapM_ printRow $ cells board

-- read nonogram file and print solved
solveFileAndPrint :: String -> IO ()
solveFileAndPrint fileName = do
  withFile fileName ReadMode
    (\handle -> do
          board <- readBoard handle
          putStrLn $ "file: " ++ fileName
          printBoard $ solveBoard board)
