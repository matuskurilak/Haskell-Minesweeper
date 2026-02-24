-- | User interface and game loop of the minesweeper game.
module Minesweeper.UI (play) where

import System.Random (newStdGen)
import Minesweeper.Board

-- | Play a random game with provided 'Solver'.
-- Result indicates if the game was solved successfully.
play :: Config -> Solver -> IO Bool
play config solver = do
    randomGen <- newStdGen
    let board = createBoard randomGen config
    gameLoop solver board

gameLoop :: Solver -> Board -> IO Bool
gameLoop solver board = do
    printBoard board
    selectedMove <- solver board
    case gameMove board selectedMove of
        (Solved, newBoard) -> solved newBoard >> return True
        (Failed, newBoard) -> failed newBoard >> return False
        (Playing, newBoard) -> gameLoop solver newBoard

solved :: Board -> IO ()
solved board = do
    printBoard board
    putStrLn "Victory! :-)"

failed :: Board -> IO ()
failed board = do
    printBoard board
    putStrLn "You lose! :-("

printBoard :: Board -> IO ()
printBoard board = putStrLn (showBoard board ++ "\n" ++ showNumMarked board)

showBoard :: Board -> String
showBoard board = unlines (zipWith (++) legendC (legendL ++ body))
  where
    (Pos height width) = boardSize board
    legendL = [concatMap (justify 3 . show) [1..width]]
    legendC = map showRowCode [0..height]
    body = map boardLine [1..height]
    boardLine i = concatMap (boardTile i) [1..width]
    boardTile row col = "  " ++ showTile (tileState board (Pos row col))

showNumMarked :: Board -> String
showNumMarked board = "Marked: " ++ show numMarked ++ "/" ++ show numMines
  where
    numMarked = boardNumMarked board
    numMines = boardNumMines board

showTile :: Tile -> String
showTile (Open x) = show x
showTile Closed   = "-"
showTile Marked   = "M"
showTile Exploded = "*"

justify :: Int -> String -> String
justify width text = replicate numSpaces ' ' ++ text
  where
    numSpaces = width - length text

showRowCode :: Int -> String
showRowCode n
    | n > 0 = [toEnum (fromEnum 'A' - 1 + n)]
    | otherwise = " "

