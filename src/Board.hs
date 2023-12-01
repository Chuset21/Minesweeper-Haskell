{-# LANGUAGE DuplicateRecordFields #-}

module Board (Board (..), BoardState (..), Cell (status), CellState (..), generateSeededBoard, generateBoard, prettyPrintBoard, prettyPrintBoardWithNumbers, getIf) where

import Control.Monad.Random
import Data.List
import qualified Data.Vector as V
import System.Random

data BoardState = Lost | Playing | Won deriving (Eq, Show)

data Cell = Cell
  { hasMine :: !Bool,
    status :: !CellState
  }
  deriving (Eq, Show)

data CellState = Flagged | Unrevealed | Revealed deriving (Eq, Show)

debugPrintCell :: Cell -> String
debugPrintCell Cell {status = Flagged} = "[F]"
debugPrintCell Cell {hasMine = m} = if m then "[M]" else "[ ]"

type Grid = V.Vector (V.Vector Cell)

data Board = Board
  { state :: !BoardState,
    grid :: !Grid
  }

prettyPrintBoard :: Board -> String
prettyPrintBoard (Board _ boardGrid) =
  unlines $ map (concatMap debugPrintCell) $ V.toList $ fmap V.toList boardGrid

prettyPrintBoardWithNumbers :: Board -> String
prettyPrintBoardWithNumbers b =
  unlines $ map (concatMap (\(i, c) -> if hasMine c then "[*]" else "[" ++ show (countSurroundingMines b i) ++ "]")) (getIndexedMatrix b)

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
  let neighbourIndices = [(x, y) | x <- [i-1..i+1],
                                    y <- [j-1..j+1],
                                    not $ (x == i) && (y == j),
                                    inBounds b (x, y) ]
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
   in (Board Playing grid, newGen)

generateBoard :: MonadRandom m => Int -> Int -> m Board
generateBoard size numBombs = do
  randoms <- getRandomRs (0, size * size - 1)
  let randomIndices = take numBombs $ nub randoms
      grid = V.generate size $ \i ->
        V.generate size $ \j ->
          Cell {hasMine = (i * size + j) `elem` randomIndices, status = Unrevealed}
  return $ Board Playing grid
