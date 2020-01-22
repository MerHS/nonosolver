{-# LANGUAGE BangPatterns #-}
module NonoSolver
    (
      Hint (..),
      Lane (..),
      Cell (..),
      Board (..),
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
import           System.IO.Unsafe (unsafePerformIO)

data Hint =
  Hint { size    :: Int
       , exRange :: Maybe (Int, Int) -- expected range
       , pivot   :: Maybe (Int, Int) -- 0-indexed pivoted cell position-1 if unpivoted
       , solved  :: Bool
       }
instance Show Hint where
  show hint =
    let
      rng = maybe "(-,-)" show $ exRange hint
      pvt = maybe "(-,-)" show $ pivot hint
    in
    (show $ size hint) ++ rng ++ pvt ++ (if solved hint then "S" else "")

data Lane =
  Lane { hints    :: [Hint]
       , modified :: Bool
       , finished :: Bool
       }
instance Show Lane where
  show lane =
    let mflag = if (modified lane) then "M" else "_"
        fflag = if (finished lane) then "F" else "_"
    in
    mflag ++ fflag ++ " " ++ (intercalate " " (map show $ hints lane))

data Cell = On | Off | Undef deriving (Eq)
instance Show Cell where
  show cell = case cell of
    On    -> "■"
    Off   -> "□"
    Undef -> "X"

data Board =
  Board { height  :: Int
        , width   :: Int
        , cells   :: [[Cell]]
        , rowHint :: [Lane]
        , colHint :: [Lane]
        }
instance Show Board where
  show board =
    let
      header = "(" ++ show (height board) ++ "," ++ show (width board) ++ ")"
      boardline = map (concat . map show) (cells board)
    in
    unlines $ header:(boardline ++ ["--rows--"] ++ map show (rowHint board) ++ ["--columns--"] ++ map show (colHint board))

------------ READER ------------

makeBlank :: Int -> Int -> [[Cell]]
makeBlank h w = replicate h (replicate w Undef)

sizeToHint :: Int -> Hint
sizeToHint s = Hint s Nothing Nothing False

readLane :: String -> Lane
readLane lane =
  Lane (map (sizeToHint . read) $ words lane) True False

readBoard' :: Handle -> IO Board
readBoard' handle = do
  whLine <- hGetLine handle
  let hw = map read $ words whLine
      h = head hw
      w = head $ tail hw
  ctn <- hGetContents handle
  if (w <= 0 || h <= 0) then fail "height or width is less than 1" else return ()
  let lanes = map readLane $ lines ctn
  if (length lanes < w + h) then fail "insufficient hint count" else return ()
  let (!rh, ch') = splitAt h lanes
  let (!ch, _) = splitAt w ch'
  return $ Board h w (makeBlank h w) rh ch

readBoard :: String -> IO Board
readBoard fileName = do
  withFile fileName ReadMode readBoard'

------------ TACTICS ------------

-- slice By Off, returns On -> True | Undef -> False List and segment left-position (zero-starting)
data Segment =
  Segment { leftPos :: Int
          , onState :: [Bool]
          } deriving (Show)

segments :: [Cell] -> [Segment]
segments lane =
  let segList = wordsBy (== Off) lane
      segState = map (map (== On)) segList
      isOffs = False:(map (/= Off) lane)
      trans = zip3 isOffs (drop 1 isOffs) [0..]
      transf = mapMaybe (\(h, t, i) -> if (not h) && t then Just i else Nothing) trans
  in
  zipWith Segment transf segState

-- Tactic 1-1. Check pivoted/solved hint & discriminate contradiction
checkPivots :: [Cell] -> Lane -> Maybe Lane
checkPivots seg lane =
  Just lane

-- Tactic 1. tactic 1-1 to 1-7


-- turnOffBlank :: [Cell] -> Lane -> [Cell]

-- solve each lane
solveLane :: Lane -> [Cell] -> Maybe (Lane, [Cell])
solveLane laneHint lane =
  Just (Lane (hints laneHint) False (finished laneHint), lane)

-- assume that lanestate are row hints.
-- transpose cellstates if you want to set column hints
setModified :: [Lane] -> [[Cell]] -> [[Cell]] -> [Lane]
setModified =
  zipWith3 $ \lane old new -> if old == new then lane else Lane (hints lane) True (finished lane)

solveByOverlap :: Board -> Maybe Board
solveByOverlap board = do
  let oldCells = cells board
  rowSolved <- zipWithM solveLane (rowHint board) oldCells

  let (rtemphint, rcells) = unzip rowSolved
      flipcells = transpose rcells
      ctemphint = setModified (colHint board) (transpose oldCells) flipcells
  colSolved <- zipWithM solveLane ctemphint flipcells

  let (chint, ccells) = unzip colSolved
      newcells = transpose ccells
      rhint = setModified rtemphint rcells newcells

  Just $ Board (height board) (width board) newcells rhint chint


solveByDFS :: Board -> Maybe Board
solveByDFS board = Just board

------------ SOLVER ------------

checkModified :: Board -> Bool
checkModified board =
  or (map modified $ rowHint board) || or (map modified $ colHint board)

checkSolved :: Board -> Bool
checkSolved board = all (notElem Undef) (cells board)

solveBoard :: Board -> Maybe Board
solveBoard board = do
  nextBoard <- solveByOverlap board
  if checkSolved nextBoard then
    Just nextBoard
  else if checkModified nextBoard then
    solveBoard board
  else
    solveByDFS nextBoard

------------ PRINTER ------------

printRow :: [Cell] -> IO ()
printRow row = do
  let squares = concat . map show $ row
  putStrLn squares

printBoard :: Board -> IO ()
printBoard board = do
  mapM_ printRow $ cells board

-- read nonogram file and print solved
solveFileAndPrint :: String -> IO ()
solveFileAndPrint fileName = do
  board <- readBoard fileName
  printBoard board
  putStrLn $ "file: " ++ fileName
  let solvedBoard = solveBoard board
  if null solvedBoard then
    putStrLn "UNSOLVABLE"
  else
    printBoard $ fromJust solvedBoard


------- TESTER (TEMP) -------

loadTest :: ([[Cell]], Board)
loadTest =
  let b = unsafePerformIO $ readBoard "test.mod"
      c = cells b
  in
  (c, b)
