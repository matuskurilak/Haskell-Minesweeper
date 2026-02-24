-- | A simple command line minesweeper game.
module Main where
import Minesweeper.Board (Config, smallBoard, mediumBoard, largeBoard)
import Minesweeper.UI (play)
import Minesweeper.HumanSolver (humanSolver)
import Minesweeper.ComputerSolver (computerSolver)

import Control.Monad (replicateM, void)
import System.Environment (getArgs)

-- | Read command line arguments and run a game or a benchmark.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> playGame smallBoard
        ["benchmark"] -> runBenchmark smallBoard 100
        ["benchmark", num] -> runBenchmark smallBoard (read num)
        [size] -> playGame (configByName size)
        [size, "benchmark"] -> runBenchmark (configByName size) 100
        [size, "benchmark", num] -> runBenchmark (configByName size) (read num)
        _ -> showUsage

showUsage :: IO ()
showUsage = do
    putStrLn "Invalid arguments."
    putStrLn "Usage: minesweeper [small|medium|large] [benchmark [runs]]"

configByName :: String -> Config
configByName "small" = smallBoard
configByName "medium" = mediumBoard
configByName "large" = largeBoard
configByName _ = error "Invalid board size. Use small, medium, or large."

-- | Play a game with human solver.
playGame :: Config -> IO ()
playGame config = void $ play config humanSolver

-- | Run benchmark of the computer solver.
runBenchmark :: Config -> Int -> IO ()
runBenchmark config num = do
    results <- replicateM num (play config computerSolver)
    let score = length (filter id results) * 100 `div` num
    putStrLn ("Computer player won " ++ show score ++ "% games")
