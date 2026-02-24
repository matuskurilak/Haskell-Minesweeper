-- | A 'Solver' asking a human player for instructions.
module Minesweeper.HumanSolver
    ( humanSolver
    ) where

import Minesweeper.Board

import Data.Char (toUpper, isSpace, ord, isAlpha, isDigit)

-- | Read a command from a user.
humanSolver :: Solver
humanSolver board = do
    putStrLn "Please enter your choice: <O A1> for open, <M A1> for mark"
    input <- getLine
    case parseInput board input of
        Left err -> putStrLn err >> humanSolver board
        Right move -> return move

-- Normalizes
parseInput :: Board -> String -> Either String Move
parseInput board input
    | null normalized   = Left "Empty input"
    | otherwise         = parseMove board normalized
    where
        normalized = normalizeInput input


-- Remove unnecessary spaces and normalize case.
normalizeInput :: String -> String
normalizeInput input = reverse (dropWhile isSpace (reverse (dropWhile isSpace (map toUpper input))))


-- Parse whole command and return either a specific error string or the Move.
-- Expects nonempty normalized string.
parseMove :: Board -> String -> Either String Move
parseMove _ [] = Left "Empty command"
parseMove board (command:rest) = do
    cmd <- parseCommand command
    pos <- parsePosition rest
    if validPos board pos
    then Right (cmd pos)
    else Left "Invalid Position"


-- Parse beginning of the move: O (for open) or M (for mark).
-- Return either an error message or Move constructor.
parseCommand :: Char -> Either String (Pos -> Move)
parseCommand 'O'    = Right OpenTile
parseCommand 'M'    = Right MarkTile
parseCommand _      = Left "Incorrect command !"

-- Parse position
parsePosition :: String -> Either String Pos
parsePosition input
    | null letters              = Left "Missing row letter"
    | length letters > 1        = Left "Too many row letters"
    | null digits               = Left "Missing column number"
    | not (all isDigit digits)  = Left "Invalid column format"
    | otherwise                 = Right (Pos (letterToRow (head letters)) (digitToNum digits))
    where
        cleaned = filter (not . isSpace) input
        (letters, digits) = span isAlpha cleaned

        letterToRow :: Char -> Int
        letterToRow char = ord char - ord 'A' + 1

        digitToNum :: String -> Int
        digitToNum = read

