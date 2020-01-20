{-# LANGUAGE BangPatterns #-}
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
       }
instance Show HintState where
  show hint =
    let
      rng = maybe "(-,-)" show $ exRange hint
      pvt = maybe "(-,-)" show $ pivot hint
    in
    (show $ size hint) ++ rng ++ pvt ++ (if solved hint then "S" else "")

data LaneState =
  Lane { hints    :: [HintState]
       , modified :: Bool
       , finished :: Bool
       }
instance Show LaneState where
  show lane =
    let mflag = if (modified lane) then "M" else "_"
        fflag = if (finished lane) then "F" else "_"
    in
    mflag ++ fflag ++ " " ++ (intercalate " " (map show $ hints lane))

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
        }
instance Show BoardState where
  show board =
    let
      header = "(" ++ show (height board) ++ "," ++ show (width board) ++ ")"
      boardline = map (concat . map show) (cells board)
    in
    unlines $ header:(boardline ++ ["--rows--"] ++ map show (rowHint board) ++ ["--columns--"] ++ map show (colHint board))

------------ READER ------------

makeBlank :: Int -> Int -> [[CellState]]
makeBlank h w = replicate h (replicate w Undef)

sizeToHint :: Int -> HintState
sizeToHint s = Hint s Nothing Nothing False

readLane :: String -> LaneState
readLane lane =
  Lane (map (sizeToHint . read) $ words lane) True False

readBoard' :: Handle -> IO BoardState
readBoard' handle = do
  whLine <- hGetLine handle
  let hw = map read $ words whLine
      h = head hw
      w = head $ tail hw
  ctn <- hGetContents handle
  if (w <= 0 || h <= 0) then fail "height or width is less than 1" else return ()
  let lanes = map readLane $ lines ctn
  if (length lanes < w + h) then fail "insufficient hint count" else return ()
  let (!rh, ch') = splitAt w lanes
  let (!ch, _) = splitAt h ch'
  return $ Board h w (makeBlank h w) rh ch

readBoard :: String -> IO BoardState
readBoard fileName = do
  withFile fileName ReadMode readBoard'

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
  let nextBoard = solveByOverlap board in
  if checkSolved nextBoard then nextBoard
  else if checkModified nextBoard then solveBoard board
  else solveByDFS nextBoard

------------ PRINTER ------------

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
  board <- readBoard fileName
  printBoard board
  putStrLn $ "file: " ++ fileName
  printBoard $ solveBoard board
