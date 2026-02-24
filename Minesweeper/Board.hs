{- | Game board definition with all helper types and functions.

The main date type is the 'Board' that contains all information playing board
and its current state. All field of the type are hidden and only accessible
using specific functions.

This module also includes the game logic in the 'gameMove' function that update
the board based on the specified 'Move'.
-}
module Minesweeper.Board
    (
    -- * Board
    -- ** Types
      Config(..)
    , Board
    , Pos(..)
    , Tile(..)
    -- ** Board creation
    , createBoard
    , smallBoard
    , mediumBoard
    , largeBoard
    -- ** Properties
    , boardSize
    , boardNumMines
    , boardNumMarked
    , validPos
    , boardPositions
    , tileState
    , isExploded
    , isOpen
    , isMarked
    , isClosed
    , neighbours

    -- * Move
    -- ** Types
    , Move(..)
    , MoveResult(..)
    , Solver
    -- ** Functions
    , gameMove
    ) where

import Data.List (nub)
import Data.Set (Set, member, notMember)  -- most used set operations
import System.Random (RandomGen, randomRs, split)

import qualified Data.Set as Set

-- Board definition ---------------------------------------------------------

-- | Configuration of the board.
data Config = Config
    { configSize :: Pos      -- ^ board size
    , configNumMines :: Int  -- ^ number of mines
    } deriving (Eq, Show)

{- | Complete state of the board. It includes the configuration and hidden
properties.

The structure of the board is not exported. To create the board, use
'createBoard' smart constructor.
 -}
data Board = Board
    { bConfig :: Config
    , bMines :: Set Pos
    , bOpened :: Set Pos
    , bMarked :: Set Pos
    , bRandPos :: [Pos]
    }

-- | Position of the tile or size of the board.
data Pos = Pos
    { posRow :: Int
    , posCol :: Int
    } deriving (Eq, Ord, Show)

-- | Tile state.
data Tile
    = Open Int -- ^ Open tile with number of neighbour mines
    | Closed   -- ^ Closed tile
    | Marked   -- ^ Tile marked for mine
    | Exploded -- ^ Exploded (opened) mine
    deriving (Eq, Show)


-- Board creation -----------------------------------------------------------

-- | Create a new board with random mines.
createBoard :: RandomGen g => g -> Config -> Board
createBoard rg config = Board
    { bConfig  = config
    , bMines   = Set.empty
    , bOpened  = Set.empty
    , bMarked  = Set.empty
    , bRandPos = randomPositions rg config
    }

randomPositions :: RandomGen g => g -> Config -> [Pos]
randomPositions rg config = nub (zipWith Pos randomRows randomCols)
  where
    randomRows = randomRs (1, height) rg1
    randomCols = randomRs (1, width) rg2
    height = posRow (configSize config) 
    width = posCol (configSize config)
    (rg1, rg2) = split rg

-- | Configuration for a small board.
smallBoard :: Config
smallBoard = Config (Pos 8 8) 10

-- | Configuration for a medium board.
mediumBoard :: Config
mediumBoard = Config (Pos 16 16) 40

-- | Configuration for a large board.
largeBoard :: Config
largeBoard = Config (Pos 16 30) 99


-- Board properties ---------------------------------------------------------

-- | Size of the board.
boardSize :: Board -> Pos
boardSize = configSize . bConfig

-- | Number of mines on the board.
boardNumMines :: Board -> Int
boardNumMines = configNumMines . bConfig

-- | Number of marked tiles on the board.
boardNumMarked :: Board -> Int
boardNumMarked = Set.size . bMarked

-- | All valid positions for the board.
boardPositions :: Board -> [Pos]
boardPositions = positionsWithinSize . boardSize
 
positionsWithinSize :: Pos -> [Pos]
positionsWithinSize size =
    [Pos r c | r <- [1..posRow size], c <- [1..posCol size]]

-- | Get state of the tile at specific position.
tileState :: Board -> Pos -> Tile
tileState board pos
    | isExploded board pos = Exploded
    | isOpen board pos     = Open (clue board pos)
    | isMarked board pos   = Marked
    | otherwise            = Closed

-- | Is tile on the position an opened mine?
isExploded :: Board -> Pos -> Bool
isExploded board pos = pos `member` bOpened board && pos `member` bMines board

-- | Is tile on the position open?
isOpen :: Board -> Pos -> Bool
isOpen board pos = pos `member` bOpened board && not (pos `member` bMines board)

-- | Is tile on the position marked?
isMarked :: Board -> Pos -> Bool
isMarked board pos = pos `member` bMarked board

-- | Is tile on the position closed?
isClosed :: Board -> Pos -> Bool
isClosed board pos =  not (isOpen board pos) && not (isMarked board pos)

-- | Get positions of neighbour tiles of specified tile.
neighbours :: Board -> Pos -> [Pos]
neighbours board center@(Pos r c) =
    filter (\p -> validPos board p && p /= center) positionsAround
  where
    positionsAround = [Pos i j | i <- [r-1..r+1], j <- [c-1..c+1]]

-- | Check if the position is within bounds of the board.
validPos :: Board -> Pos -> Bool
validPos board (Pos row col) =
    row >= 1 && row <= maxRow && col >= 1 && col <= maxCol
  where
    (Pos maxRow maxCol) = boardSize board


-- Move definition ----------------------------------------------------------

-- | Types of moves available for the player.
data Move = MarkTile Pos  -- ^ Mark or unmark the tile on specified position
          | OpenTile Pos  -- ^ Open the tile on specified position

-- | Result of the move.
data MoveResult
    = Playing  -- ^ Game continues
    | Solved   -- ^ Game was solved and the player wins
    | Failed   -- ^ The player hits a mine and loses

-- | Solver is just a function from 'Board' to 'Move' in IO monad.
type Solver = Board -> IO Move


-- Game logic ---------------------------------------------------------------

-- | Execute the move and give the result and updated board.
-- This function implements main logic of the game.
gameMove :: Board -> Move -> (MoveResult, Board)
gameMove board (MarkTile pos) = moveMark board pos
gameMove board (OpenTile pos) = moveOpen board pos

moveMark :: Board -> Pos -> (MoveResult, Board)
moveMark board@(Board{bMarked=marked, bOpened=opened}) pos
    | pos `member` opened = (Playing, board)
    | pos `notMember` marked = (Playing, markedBoard)
    | otherwise = (Playing, unmarkedBoard)
  where
    markedBoard = board {bMarked = Set.insert pos marked}
    unmarkedBoard = board {bMarked = Set.delete pos marked}

moveOpen :: Board -> Pos -> (MoveResult, Board)
moveOpen board pos
    | firstOpen = (Playing, openTile (initBoard board pos) pos)
    | failed = (Failed, openBoard board)
    | solved = (Solved, newBoard)
    | otherwise = (Playing, newBoard)
  where
    newBoard = openTile board pos
    failed = pos `member` bMines board
    solved = bClosed newBoard == bMines newBoard
    firstOpen = null (bOpened board)

bClosed :: Board -> Set Pos
bClosed board = Set.difference allPositions (bOpened board)
  where
    allPositions = Set.fromList (boardPositions board)

openBoard :: Board -> Board
openBoard board = board { bOpened = Set.fromList (boardPositions board) }

initBoard :: Board -> Pos -> Board
initBoard board first = board { bMines = Set.fromList mines }
  where
    mines = takeNeeded $ removeFirstNeighbours $ bRandPos board
    takeNeeded = take (boardNumMines board)
    removeFirstNeighbours = filter (`notElem` tilesToSkip)
    tilesToSkip = first : neighbours board first

clue :: Board -> Pos -> Int
clue board pos = length (filter isMine (neighbours board pos))
  where
    isMine p = p `member` bMines board

openTile :: Board -> Pos -> Board
openTile board pos
    | pos `member` bOpened board = board
    | clue board pos == 0 = foldl openTile openedTile (neighbours board pos)
    | otherwise = openedTile
  where
    openedTile = board 
        { bOpened = Set.insert pos (bOpened board)
        , bMarked = Set.delete pos (bMarked board) }

