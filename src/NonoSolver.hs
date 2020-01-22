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
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

data Hint =
  Hint { getSize    :: Int -- range is zero-indexed, closed
       , getExRange :: Maybe (Int, Int) -- expected range
       , getPivot   :: Maybe (Int, Int) -- pivoted cell position
       , isSolved   :: Bool
       }
instance Show Hint where
  show hint =
    let
      rng = maybe "(-,-)" show $ getExRange hint
      pvt = maybe "(-,-)" show $ getPivot hint
    in
    (show $ getSize hint) ++ rng ++ pvt ++ (if isSolved hint then "S" else "")

data Lane =
  Lane { getHints   :: [Hint]
       , isModified :: Bool
       , isFinished :: Bool
       }
instance Show Lane where
  show lane =
    let mflag = if (isModified lane) then "M" else "_"
        fflag = if (isFinished lane) then "F" else "_"
    in
    mflag ++ fflag ++ " " ++ (intercalate " " (map show $ getHints lane))

data Cell = On | Off | Undef deriving (Eq)
instance Show Cell where
  show cell = case cell of
    On    -> "■"
    Off   -> "□"
    Undef -> "X"

data Board =
  Board { getHeight   :: Int
        , getWidth    :: Int
        , getLattice  :: [[Cell]]
        , getRowHints :: [Lane]
        , getColHints :: [Lane]
        }
instance Show Board where
  show board =
    let
      header = "(" ++ show (getHeight board) ++ "," ++ show (getWidth board) ++ ")"
      boardline = map (concat . map show) (getLattice board)
    in
    unlines $ header:(boardline ++ ["--rows--"] ++ map show (getRowHints board) ++ ["--columns--"] ++ map show (getColHints board))

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
  if (w <= 0 || h <= 0) then fail "getHeight or getWidth is less than 1" else return ()
  let lanes = map readLane $ lines ctn
  if (length lanes < w + h) then fail "insufficient hint count" else return ()
  let (!rh, ch') = splitAt h lanes
  let (!ch, _) = splitAt w ch'
  return $ Board h w (makeBlank h w) rh ch

readBoard :: String -> IO Board
readBoard fileName = do
  withFile fileName ReadMode readBoard'

------------ UTILITY ------------

filterMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
filterMap f m = map (\ax -> if f ax then m ax else ax)

------------ TACTICS ------------

-- slice By Off, returns On -> True | Undef -> False List and segment left-position (zero-starting)
data Segment =
  Segment { getLeftPos :: Int
          , getOnState :: [Bool]
          } deriving (Show)

segmentize :: [Cell] -> [Segment]
segmentize lane =
  let segList = wordsBy (== Off) lane
      segState = map (map (== On)) segList
      isOffs = False:(map (/= Off) lane)
      trans = zip3 isOffs (drop 1 isOffs) [0..]
      transf = mapMaybe (\(h, t, i) -> if (not h) && t then Just i else Nothing) trans
  in
  zipWith Segment transf segState

desegmentize :: Int -> [Segment] -> [Cell]
desegmentize laneSize segments = elems $ runSTArray $ do
    cells <- newArray (0, laneSize - 1) Off
    forM_ segments $ \(Segment lp on) -> do
      forM_ (zip on [0..]) $ \(isOn, segId) -> do
        writeArray cells (lp + segId) (if isOn then On else Undef)
    return cells

segSizes :: [Segment] -> [(Int, Int)]
segSizes = map (\seg -> (getLeftPos seg, length $ getOnState seg))

-- return hint by segment getSize and range (overridable isSolved)
segToHint :: Bool -> Segment -> Hint
segToHint overrideSolve seg =
  let len = length $ getOnState seg
      lpos = getLeftPos seg
      solved = overrideSolve || and (getOnState seg)
  in
    Hint len (Just (lpos, lpos + len - 1)) (Just (lpos, lpos + len - 1)) solved

-- Tactic 1. Solve Trivial Cases
pivotOnSet :: Int -> [Hint] -> [Cell]
pivotOnSet len hints = elems $ runSTArray $ do
  cells <- newArray (0, len - 1) Off
  forM_ hints $ \(Hint _ _ pivot _) ->
    case pivot of
      Nothing -> return ()
      Just (l, r) ->
        forM_ [l..r] $ \i ->
          writeArray cells i On
  return cells

findConflict :: [Cell] -> [Cell] -> Maybe [Cell]
findConflict old new =
  if or (zipWith isConflict old new) then
    Nothing
  else
    Just new
  where isConflict cold cnew = (cold == On && cnew == Off) || (cold == Off && cnew == On)

solveTrivial :: Lane -> [Cell] -> Maybe (Lane, [Cell])
solveTrivial lane cells =
  let
    segments = segmentize cells
    hints = getHints lane
    hintTotal = length hints + (sum $ map getSize hints)
    segTotal = length segments + (sum $ map (length . getOnState) segments)
  in
  if hintTotal /= segTotal then
    Just (lane, cells)
  else do
    allocedHints <- allocateHints hints segments []
    cellConflict <- findConflict cells (pivotOnSet (length cells) allocedHints)
    return $ (Lane allocedHints False True, cellConflict)

allocateHints :: [Hint] -> [Segment] -> [Hint] -> Maybe [Hint]
allocateHints hints segments hintAcc =
  case hints of
    [] -> Just $ reverse hintAcc
    (Hint size _ _ _):hs -> case segments of
      []     -> Nothing
      (Segment lp onState):ss ->
        if (length onState < size || length onState == size + 1) then
          Nothing
        else
          let newAcc = (Hint size (Just (lp, lp + size - 1)) (Just (lp, lp + size - 1)) True):hintAcc in
          if length onState > size then
            allocateHints hs ((Segment (lp + size + 1) (drop (size + 1) onState)):ss) newAcc
          else
            allocateHints hs ss newAcc


-- Tactic 2-1. Check pivoted/isSolved hint & discriminate contradiction
checkPivots :: Lane -> [Cell] -> Maybe Lane
checkPivots lane cells =
  Just lane

-- turnOffBlank :: [Cell] -> Lane -> [Cell]

------------ TACTIC MERGER -----------

-- resolve all getHints by isSolved cell
finalizeLane :: Lane -> [Cell] -> Maybe (Lane, [Cell])
finalizeLane lane cells =
  if not $ isFinished lane then
    Just (lane, cells)
  else if all isSolved (getHints lane) then
    Just (lane, cells)
  else
    let
      hints = getHints lane
      segments = segmentize cells
    in
    if length segments /= length hints then
      Nothing
    else if any (\(hint, seg) -> getSize hint /= length (getOnState seg)) (zip hints segments) then
      Nothing
    else
      let newHints = map (segToHint True) segments in
      Just (Lane newHints False True, cells)

-- apply tactic when the lane is not isFinished
applyTactic :: (Lane -> [Cell] -> Maybe (Lane, [Cell])) -> Lane -> [Cell] -> Maybe (Lane, [Cell])
applyTactic tactic lane cells =
  if isFinished lane then
    finalizeLane lane cells
  else do
    (l1, c1) <- tactic lane cells
    finalizeLane l1 c1

-- solve each lane
solveLane :: Lane -> [Cell] -> Maybe (Lane, [Cell])
solveLane lane cells =
  if isFinished lane || not (isModified lane) then
    Just (lane, cells)
  else
    do
      (lane1, cells1) <- applyTactic solveTrivial lane cells
      return (lane1, cells1)


-- assume that lanestate are row getHints.
-- transpose cellstates if you want to set column getHints
setModified :: [[Cell]] -> [[Cell]] -> [Lane] -> [Lane]
setModified =
  zipWith3 $ \old new lane ->
    if isFinished lane || old == new then
      Lane (getHints lane) False True
    else
      Lane (getHints lane) True (isFinished lane)

solveByOverlap :: Board -> Maybe Board
solveByOverlap board = do
  let oldLattice = getLattice board
  rowSolved <- zipWithM solveLane (getRowHints board) oldLattice

  let (rtemphint, rcells) = unzip rowSolved
      flipcells = transpose rcells
      ctemphint = setModified (transpose oldLattice) flipcells $ getColHints board
  colSolved <- zipWithM solveLane ctemphint flipcells

  let (chint, ccells) = unzip colSolved
      newcells = transpose ccells
      rhint = setModified rcells newcells rtemphint

  Just $ Board (getHeight board) (getWidth board) newcells rhint chint


solveByDFS :: Board -> Maybe Board
solveByDFS board = Just board

------------ SOLVER ------------

checkModified :: Board -> Bool
checkModified board =
  or (map isModified $ getRowHints board) || or (map isModified $ getColHints board)

checkSolved :: Board -> Bool
checkSolved board = all (notElem Undef) (getLattice board)

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
  mapM_ printRow $ getLattice board

-- read nonogram file and print isSolved
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
      c = getLattice b
  in
  (c, b)
