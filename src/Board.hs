{-# LANGUAGE DuplicateRecordFields #-}

module Board (Board (..), BoardState (..), Cell (status), CellState (..), generateSeededBoard, generateBoard, prettyPrintBoard, getIf, getTotalMines) where

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

-- Length and width will be equal
size :: Board -> Int
size Board {grid = g} = V.length g

getIf :: (Cell -> Bool) -> Board -> [((Int, Int), Cell)]
getIf predicate Board {grid = g} =
  [((i, j), cell) | (i, row) <- zip [0 ..] (V.toList g), (j, cell) <- zip [0 ..] (V.toList row), predicate cell]

getTotalMines :: Board -> Int
getTotalMines board = length $ getIf hasMine board

getCellAtIndex :: (Int, Int) -> Board -> Maybe Cell
getCellAtIndex (i, j) Board {grid = g} = do
  row <- g V.!? i
  row V.!? j

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
