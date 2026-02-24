-- | A 'Solver' playing automatically using artificial inteligence.
module Minesweeper.ComputerSolver (computerSolver) where

import Minesweeper.Board
import Data.List (nub)


-- | Automatically determine next move.
computerSolver :: Solver
computerSolver board =
    if null openTiles
    then return (OpenTile (getCenter board))
    else case findSafeMove board of
        Just move -> return move
        Nothing -> 
            if (length(closedTiles) == remainingMines)
            then return (MarkTile (head closedTiles))
            else return (OpenTile (findSafestTile board (getClosedTiles board)))
    where
        openTiles = filter (isOpen board) (boardPositions board)
        closedTiles = getClosedTiles board
        remainingMines = boardNumMines board - boardNumMarked board


-- Traverses all open tiles and applies algorithms, which finds first safe move
findSafeMove :: Board -> Maybe Move
findSafeMove board = 
    case foldr tryTile Nothing openTiles of
        Just move -> Just move
        Nothing -> findSafeByConstraints board
    where
        openTiles = filter (isOpen board) (boardPositions board)

        tryTile :: Pos -> Maybe Move -> Maybe Move
        tryTile pos acc =
            case acc of
                Just _ -> acc
                Nothing -> applyRulesForTile board pos


-- Checks rules A + B for each opened tile and returns a safe move or nothing 
applyRulesForTile :: Board -> Pos -> Maybe Move
applyRulesForTile board pos = 
    case tileState board pos of
        Open n -> 
            if countClosed == (n - countMarked) && countClosed > 0
            then Just (MarkTile (head closedNeighbours))
            else if countMarked == n && countClosed > 0
                then Just (OpenTile (head closedNeighbours))
                else Nothing
            where
                allNeighbours = neighbours board pos

                markedNeighbours = filter (isMarked board) allNeighbours
                closedNeighbours = filter (isClosed board) allNeighbours

                countMarked = length markedNeighbours
                countClosed = length closedNeighbours
        _ -> Nothing


-- constraint type
data Constraint = Constraint
    { constraintCells :: [Pos]  -- Closed neighbouring cells
    , constraintMines :: Int     -- Number of mines among them
    } deriving Eq

-- get constraints for each open tile 
buildConstraints :: Board -> [Constraint]
buildConstraints board = foldr addConstraint [] openTiles
    where
        openTiles = filter (isOpen board) (boardPositions board)

        addConstraint :: Pos -> [Constraint] -> [Constraint]
        addConstraint pos acc =
            case buildConstraintForTile board pos of
                Just c -> c:acc
                Nothing -> acc

-- returns (mine positions, safe positions) for each constraint pair
solveConstraints :: [Constraint] -> (Maybe [Pos], Maybe [Pos])
solveConstraints constraints = 
    case (null allMines, null allSafe) of
        (False, False) -> (Just allMines, Just allSafe)
        (False, True) -> (Just allMines, Nothing)
        (True, False) -> (Nothing, Just allSafe)
        (True, True) -> (Nothing, Nothing)
    where
        pairs = [(c1, c2) | c1 <- constraints, c2 <- constraints, c1 /= c2]
        results = [ analyzePair c1 c2 | (c1, c2) <- pairs]

        (minesList, safeList) = unzip results
        allMines = nub (concat minesList)
        allSafe = nub (concat safeList)

-- driver function for constraint satisfaction principle
findSafeByConstraints :: Board -> Maybe Move
findSafeByConstraints board = 
    case solveConstraints constraints of
        (Just mines, _) -> Just (MarkTile (head mines))
        (Nothing, Just safe) -> Just (OpenTile (findTileWithMostOpenNeighbours board safe))
        (Nothing, Nothing) -> Nothing 
    where
        constraints = buildConstraints board

-- Counts global probability of a tile containing a mine
globalProbability :: Board -> Double
globalProbability board = fromIntegral (totalMines - totalMarked) / fromIntegral totalClosed
    where
        totalMines = boardNumMines board
        totalMarked = boardNumMarked board
        totalClosed = length (getClosedTiles board)

-- get local probability of a neighbours of a open tile
tileLocalProbability :: Board -> Pos -> Maybe Double
tileLocalProbability board pos = 
    case tileState board pos of
        Open n -> 
            if null closedNeighbours
            then Nothing
            else Just (fromIntegral (n - markedCount) / fromIntegral closedCount)
        _ -> Nothing
    where
        allNeighbours = neighbours board pos
        markedNeighbours = filter (isMarked board) allNeighbours
        closedNeighbours = filter (isClosed board) allNeighbours
        markedCount = length markedNeighbours
        closedCount = length closedNeighbours


-- counts mine probability of one closed tile
tileProbability :: Board -> Pos -> Double
tileProbability board pos =
    if null validContributions
    then globalProbability board
    else combinedProbability validContributions
    where
        openNeighbours = filter (isOpen board) (neighbours board pos)
        -- counts contribution to probability of given open neighbor
        contributions = map (tileLocalProbability board) openNeighbours
        validContributions = [x | Just x <- contributions]

        combinedProbability :: [Double] -> Double
        combinedProbability probs = 1 - product (map (1 -) probs)
    


-- Chooses tile with lowest probability of containing a mine from a list of closed tiles
findSafestTile :: Board -> [Pos] -> Pos
findSafestTile board closedTiles =
    foldr comparePositions (head tilesToConsider) tilesToConsider 
    where 
        {-
        -- TODO: experiment with restricting search to frontier tiles only
        frontierTiles = filter hasSomeOpenNeighbour closedTiles
        tilesToConsider =   if null frontierTiles
                            then closedTiles
                            else frontierTiles
        -}
        tilesToConsider = closedTiles
        hasSomeOpenNeighbour :: Pos -> Bool
        hasSomeOpenNeighbour pos = any (isOpen board) (neighbours board pos)

        comparePositions :: Pos -> Pos -> Pos
        comparePositions pos1 pos2
            | tileProbability board pos1 < tileProbability board pos2 = pos1
            | tileProbability board pos1 > tileProbability board pos2 = pos2
            | otherwise =   if countOpenNeighbours pos1 > countOpenNeighbours pos2
                            then pos1
                            else pos2

        countOpenNeighbours :: Pos -> Int
        countOpenNeighbours pos = length (filter (isOpen board) (neighbours board pos))       


------------------- HELPER FUNCTIONS ------------------

-- Traverses through all positions and returns closed ones
getClosedTiles :: Board -> [Pos]
getClosedTiles board = filter (isClosed board) (boardPositions board)


-- Gets center of game board
getCenter :: Board -> Pos
getCenter board = Pos (maxRow `div` 2) (maxCol `div` 2)
    where
        Pos maxRow maxCol = boardSize board


-- Creates constraint for one open tile
buildConstraintForTile :: Board -> Pos -> Maybe Constraint
buildConstraintForTile board pos =
    case tileState board pos of
        Open n ->
            if null closed
            then Nothing
            else Just (Constraint closed remainingMines)
            where
                allNeighbours = neighbours board pos
                closed = filter (isClosed board) allNeighbours
                marked = filter (isMarked board) allNeighbours
                remainingMines = n - length marked
        _ -> Nothing


-- checks if [Pos] is subset of another [Pos]
isSubsetOf :: [Pos] -> [Pos] -> Bool
isSubsetOf subset superset = all isInSuperset subset
    where
        isInSuperset x = elem x superset

-- checks difference between subsets and filters it out
difference :: [Pos] -> [Pos] -> [Pos]
difference subset superset = filter isDifferent subset
    where
        isDifferent x = notElem x superset


-- checks constraint cells and analyzes it
analyzePair :: Constraint -> Constraint -> ([Pos], [Pos])
analyzePair c1 c2 =
    case isSubsetOf (constraintCells c1) (constraintCells c2) of
        True ->
            if diffMines == 0
            then ([], diffCells)
            else if diffMines == length diffCells
                then (diffCells, [])
                else ([], [])
        _ -> ([], [])
    where    
        diffCells = difference (constraintCells c2) (constraintCells c1)
        diffMines = constraintMines c2 - constraintMines c1

-- finds a tile with most open neighbours from the [Pos]
findTileWithMostOpenNeighbours :: Board -> [Pos] -> Pos
findTileWithMostOpenNeighbours board tiles = 
    foldr findMaxNeighbours (head tiles) tiles
    where
        countOpenNeighbours :: Pos -> Int
        countOpenNeighbours pos = length (filter (isOpen board) (neighbours board pos))

        findMaxNeighbours :: Pos -> Pos -> Pos
        findMaxNeighbours pos1 pos2
            | countOpenNeighbours pos1 > countOpenNeighbours pos2 = pos1
            | otherwise = pos2



