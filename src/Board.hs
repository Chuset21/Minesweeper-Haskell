{-# LANGUAGE DuplicateRecordFields #-}

module Board
  ( Board (state),
    BoardState (..),
    VisualBoard (..),
    VisualState (..),
    toggleFlag,
    revealCell,
    getBoardVisuals,
    generateSeededBoard,
    generateBoard,
    inBounds,
    size,
    prettyPrintBoardWithNumbers, -- TODO remove
  )
where

import Control.Monad.Random
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import System.Random

data BoardState = Lost | Playing | Won deriving (Eq, Show)

data Cell = Cell
  { hasMine :: !Bool,
    status :: !CellState
  }
  deriving (Eq, Show)

data CellState = Flagged_ | Unrevealed | Revealed deriving (Eq, Show)

type Grid = V.Vector (V.Vector Cell)

data Board = Board
  { state :: !BoardState,
    grid :: !Grid,
    totalMines :: !Int
  }
  deriving (Eq, Show)

data VisualState = Covered | Uncovered | Flagged | Exploded | SurroundingMines !Int deriving (Eq, Show)

data VisualBoard = VisualBoard
  { state :: !BoardState,
    grid :: ![[((Int, Int), VisualState)]]
  }
  deriving (Eq, Show)

prettyPrintBoardWithNumbers :: Board -> String
prettyPrintBoardWithNumbers b =
  unlines $ map (concatMap (\(i, c) -> if hasMine c then "[*]" else "[" ++ show (countSurroundingMines b i) ++ "]")) (getIndexedMatrix b)

getBoardVisuals :: Board -> VisualBoard
getBoardVisuals b@Board {state = s} =
  VisualBoard {state = s, grid = map (map (\c@(i, _) -> (i, cellToVisualState b c))) (getIndexedMatrix b)}

cellToVisualState :: Board -> ((Int, Int), Cell) -> VisualState
cellToVisualState b@Board {state = s} (indices, cell) =
  case (s, status cell) of
    (Playing, Unrevealed) -> Covered
    (Playing, Revealed) -> getRevealedValue
    (Playing, Flagged_) -> Flagged
    (Won, st) -> case st of
      Revealed -> getRevealedValue
      _ -> Flagged
    (Lost, st) ->
      if hasMine cell
        then Exploded
        else case st of
          Revealed -> getRevealedValue
          Unrevealed -> Covered
          Flagged_ -> Flagged
  where
    getRevealedValue =
      let n = countSurroundingMines b indices
       in if n > 0 then SurroundingMines n else Uncovered

-- Length and width will be equal
size :: Board -> Int
size Board {grid = g} = V.length g

getIf :: Board -> (Cell -> Bool) -> [((Int, Int), Cell)]
getIf Board {grid = g} predicate =
  [((i, j), cell) | (i, row) <- V.toList $ V.indexed g, (j, cell) <- V.toList $ V.indexed row, predicate cell]

-- Get a flattened list of all the cells with indices
getIndexedList :: Board -> [((Int, Int), Cell)]
getIndexedList b = getIf b $ const True

-- Get a matrix of all the cells with indices
getIndexedMatrix :: Board -> [[((Int, Int), Cell)]]
getIndexedMatrix b = groupBy (\((x, _), _) ((y, _), _) -> x == y) (getIndexedList b) -- We want to separate the list by row, so whenever the row isn't equal, separate it onto a new list

getCellAtIndexUnsafe :: Board -> (Int, Int) -> Cell
getCellAtIndexUnsafe Board {grid = g} (i, j) = (g V.! i) V.! j

getCellAtIndex :: Board -> (Int, Int) -> Maybe Cell
getCellAtIndex Board {grid = g} (i, j) = do
  row <- g V.!? i
  row V.!? j

inBounds :: Board -> (Int, Int) -> Bool
inBounds b (i, j) =
  let h = size b
   in 0 <= i && 0 <= j && i < h && j < h

getNeighbours :: Board -> (Int, Int) -> [((Int, Int), Cell)]
getNeighbours b (i, j) =
  let neighbourIndices =
        [ (x, y) | x <- [i -1 .. i + 1], y <- [j -1 .. j + 1], not $ (x == i) && (y == j), inBounds b (x, y)
        ]
   in map (\i -> (i, getCellAtIndexUnsafe b i)) neighbourIndices

getTotalMines :: [Cell] -> Int
getTotalMines cells = length $ filter hasMine cells

countSurroundingMines :: Board -> (Int, Int) -> Int
countSurroundingMines b indices = getTotalMines $ map snd $ getNeighbours b indices

generateSeededBoard :: RandomGen g => g -> Int -> Int -> (Board, g)
generateSeededBoard gen size numBombs =
  let (seed, newGen) = split gen
      randomIndices = take numBombs $ nub $ randomRs (0, size * size - 1) seed
      grid = V.generate size $ \i ->
        V.generate size $ \j ->
          Cell {hasMine = (i * size + j) `elem` randomIndices, status = Unrevealed}
   in (Board Playing grid numBombs, newGen)

generateBoard :: MonadRandom m => Int -> Int -> m Board
generateBoard size numBombs = do
  randoms <- getRandomRs (0, size * size - 1)
  let randomIndices = take numBombs $ nub randoms
      grid = V.generate size $ \i ->
        V.generate size $ \j ->
          Cell {hasMine = (i * size + j) `elem` randomIndices, status = Unrevealed}
  return $ Board Playing grid numBombs

updateCellStatus :: (Cell -> CellState) -> Board -> (Int, Int) -> Board
updateCellStatus getNewState b@Board {grid = g} (i, j) =
  b {grid = newGrid}
  where
    oldRow = g V.! i
    oldCell = oldRow V.! j
    newCell = oldCell {status = getNewState oldCell}
    newRow = V.update oldRow (V.singleton (j, newCell))
    newGrid = V.update g (V.singleton (i, newRow))

revealCell :: Board -> (Int, Int) -> Board
revealCell b i = maybe b (\c -> if status c == Unrevealed then revealCellHelper b i else b) (getCellAtIndex b i)

-- This function reveals a cell and if it has no surrounding mines, it reveals all the cells around it, recursively
revealNecessaryCells :: Board -> (Int, Int) -> Board
revealNecessaryCells b i =
  let newBoard = updateCellStatus (const Revealed) b i
   in if countSurroundingMines b i == 0
        then foldl' revealNecessaryCells newBoard (map fst $ filter (\(i, c) -> status c /= Revealed) (getNeighbours newBoard i)) -- Reveal all cells around it, make sure to not include revealed cells or infinite recursion... hard to find bug :(
        else newBoard

-- This should not be called if the cell is already revealed or flagged, it assumes it is unrevealed and that the game state is Playing
revealCellHelper :: Board -> (Int, Int) -> Board
revealCellHelper b@Board {state = s, totalMines = mines} i =
  newBoard {state = newBoardState}
  where
    newBoard = revealNecessaryCells b i

    totalNumberOfCells = size b * size b
    newBoardState
      | hasMine $ getCellAtIndexUnsafe b i = Lost
      | (totalNumberOfCells - mines) <= length (getIf b (\c -> status c == Revealed)) + 1 = Won -- + 1 because this is the old board
      | otherwise = s

toggleFlag :: Board -> (Int, Int) -> Board
toggleFlag b i = if inBounds b i then toggleFlagHelper b i else b

toggleFlagHelper :: Board -> (Int, Int) -> Board
toggleFlagHelper =
  updateCellStatus
    ( \c -> case status c of
        Unrevealed -> Flagged_
        Flagged_ -> Unrevealed
        _ -> status c
    )