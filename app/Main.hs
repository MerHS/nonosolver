module Main where

import           NonoSolver
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ solveFileAndPrint args
