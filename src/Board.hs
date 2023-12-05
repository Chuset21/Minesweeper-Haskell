{-# LANGUAGE DuplicateRecordFields #-}

module Board
  ( Board (state, totalMines),
    BoardState (..),
    VisualBoard (..),
    VisualState (..),
    Boarded (size),
    inBounds,
    toggleFlag,
    revealCell,
    getBoardVisuals,
    generateSeededBoard,
    generateBoard
  )
where

import Control.Monad.Random
  ( MonadRandom (getRandomRs),
    Random (randomRs),
  )
import Data.List (foldl', groupBy, nub)
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import System.Random (Random (randomRs), RandomGen (split))

-- Board state
data BoardState = Lost | Playing | Won deriving (Eq, Show)

-- Information about a Cell, this is only to be used within this module, it is not exported
data Cell = Cell
  { hasMine :: !Bool,
    status :: !CellState
  }
  deriving (Eq, Show)

-- Information about a cell's state
data CellState = Flagged_ | Unrevealed | Revealed deriving (Eq, Show)

-- Type class for different boards
class Boarded b where
  size :: b -> Int

-- Board type, inside this module we can check the contents of the grid and manipulate it, outside users can't
data Board = Board
  { state :: !BoardState,
    grid :: !(V.Vector (V.Vector Cell)),
    totalMines :: !Int
  }
  deriving (Eq, Show)

-- The state that the user sees a cell as, this is to be used to show the state of the game is in conjuction with VisualBoard
-- Users can then use this to create programs that play the game, or play it themselves with some sort of GUI or visual representation
data VisualState = Covered | Uncovered | Flagged | Exploded | SurroundingMines !Int deriving (Eq, Show, Ord)

-- A visual board, this is exported and can be used to see the information about the game
-- This ensures that cheating and checking if there is a mine at a square is not possible
-- Note: since data is immutable, it is still possible for agents to play a move, if it is a mine go back to the previous state and play a different move,
--       however you can easily check if this is done, just by checking if the agent imports or uses the "revealCell" function
data VisualBoard = VisualBoard
  { state :: !BoardState,
    grid :: ![[((Int, Int), VisualState)]],
    totalMines :: !Int
  }
  deriving (Eq, Show)

instance Boarded Board where
  -- Length and width will be equal
  size Board {grid = g} = V.length g

instance Boarded VisualBoard where
  -- Length and width will be equal
  size VisualBoard {grid = g} = length g

-- Check whether an index (i, j) is in bounds
inBounds :: Boarded b => b -> (Int, Int) -> Bool
inBounds b (i, j) = 0 <= i && 0 <= j && i < h && j < h
  where
    h = size b

-- Map a board to a visual board
getBoardVisuals :: Board -> VisualBoard
getBoardVisuals b@Board {state = s, totalMines = m} =
  VisualBoard {state = s, grid = map (map (\c@(i, _) -> (i, cellToVisualState b c))) (getIndexedMatrix b), totalMines = m}

-- Map a cell in a board to a visual state
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

-- Get a list of indexed cells that satisfy a condition
getIf :: Board -> (Cell -> Bool) -> [((Int, Int), Cell)]
getIf Board {grid = g} predicate =
  [((i, j), cell) | (i, row) <- V.toList $ V.indexed g, (j, cell) <- V.toList $ V.indexed row, predicate cell]

-- Get a flattened list of all the cells with indices
getIndexedList :: Board -> [((Int, Int), Cell)]
getIndexedList b = getIf b $ const True

-- Get a matrix of all the cells with indices
getIndexedMatrix :: Board -> [[((Int, Int), Cell)]]
getIndexedMatrix b = groupBy (\((x, _), _) ((y, _), _) -> x == y) (getIndexedList b) -- We want to separate the list by row, so whenever the row isn't equal, separate it onto a new list

-- Get a cell at an index unsafely
getCellAtIndexUnsafe :: Board -> (Int, Int) -> Cell
getCellAtIndexUnsafe Board {grid = g} (i, j) = (g V.! i) V.! j

-- Get a cell at an index safely
getCellAtIndex :: Board -> (Int, Int) -> Maybe Cell
getCellAtIndex Board {grid = g} (i, j) = do
  row <- g V.!? i
  row V.!? j

-- Get a cell's neighbours, only the index of the cell is needed, not the actual cell
getNeighbours :: Board -> (Int, Int) -> [((Int, Int), Cell)]
getNeighbours b (i, j) =
  let neighbourIndices =
        [ (x, y) | x <- [i -1 .. i + 1], y <- [j -1 .. j + 1], not $ (x == i) && (y == j), inBounds b (x, y)
        ]
   in map (\i -> (i, getCellAtIndexUnsafe b i)) neighbourIndices

-- Get the total amount of mines in a list of cells
getTotalMines :: [Cell] -> Int
getTotalMines cells = length $ filter hasMine cells

-- Count the number of mines that surround a cell
countSurroundingMines :: Board -> (Int, Int) -> Int
countSurroundingMines b indices = getTotalMines $ map snd $ getNeighbours b indices

-- Generate a seeded board, this is currently not used
generateSeededBoard :: RandomGen g => g -> Int -> Int -> (Board, g)
generateSeededBoard gen size numBombs =
  let (seed, newGen) = split gen
      randomIndices = take numBombs $ nub $ randomRs (0, size * size - 1) seed
      grid = V.generate size $ \i ->
        V.generate size $ \j ->
          Cell {hasMine = (i * size + j) `elem` randomIndices, status = Unrevealed}
   in (Board Playing grid numBombs, newGen)

-- Generate a board inside the MonadRandom
generateBoard :: MonadRandom m => Int -> Int -> m Board
generateBoard size numBombs = do
  randoms <- getRandomRs (0, size * size - 1)
  let randomIndices = take numBombs $ nub randoms
      grid = V.generate size $ \i ->
        V.generate size $ \j ->
          Cell {hasMine = (i * size + j) `elem` randomIndices, status = Unrevealed}
  pure $ Board Playing grid numBombs

-- Update a cell status and return the new grid containing the new cell
updateCellStatus :: (Cell -> CellState) -> Board -> (Int, Int) -> Board
updateCellStatus getNewState b@Board {grid = g} (i, j) =
  b {grid = newGrid}
  where
    oldRow = g V.! i
    oldCell = oldRow V.! j
    newCell = oldCell {status = getNewState oldCell}
    newRow = V.update oldRow (V.singleton (j, newCell))
    newGrid = V.update g (V.singleton (i, newRow))

-- Users call this to reveal a cell
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
revealCellHelper b@Board {state = s, totalMines = mines} ind =
  -- If it is the first move then it must be safe
  if isMine && isFirstMove && isJust maybeIndexWithNoMine -- If first two conditions are met but no cell with no mine was found then they are all mines...
    then
      let indexWithNoMine = fromJust maybeIndexWithNoMine -- Find index with no mine
          boardWithMineAtIndex = setCellAtIndex b (indexWithNoMine, Cell {hasMine = True, status = Unrevealed}) -- Put a mine at the index without a mine
          newBoard = setCellAtIndex boardWithMineAtIndex (ind, Cell {hasMine = False, status = Unrevealed}) -- Remove the mine at the current index
       in revealCellHelper newBoard ind -- Play the move with new board
    else
      let newBoard = revealNecessaryCells b ind
          newBoardState
            | hasMine $ getCellAtIndexUnsafe b ind = Lost
            | (size b * size b - mines) <= length (getIf newBoard (\c -> status c == Revealed)) = Won
            | otherwise = s
       in newBoard {state = newBoardState}
  where
    isFirstMove = size b * size b == length (getIf b (\c -> status c == Unrevealed))
    isMine = hasMine $ getCellAtIndexUnsafe b ind
    maybeIndexWithNoMine = tryFindIndexWithNoMine b

-- Users call this to toggle a covered square from being flagged or not
toggleFlag :: Board -> (Int, Int) -> Board
toggleFlag b i = if inBounds b i then toggleFlagHelper b i else b

toggleFlagHelper :: Board -> (Int, Int) -> Board
toggleFlagHelper =
  updateCellStatus
    ( \c -> case status c of
        Unrevealed -> Flagged_
        Flagged_ -> Unrevealed
        x -> x
    )

-- Try to find an index without a mine
tryFindIndexWithNoMine :: Board -> Maybe (Int, Int)
tryFindIndexWithNoMine b@Board {grid = g} = do
  i <- V.findIndex (V.any (not . hasMine)) g
  j <- V.findIndex (not . hasMine) (g V.! i)
  pure (i, j)

-- Set cell at a particular index to a new value
setCellAtIndex :: Board -> ((Int, Int), Cell) -> Board
setCellAtIndex b@Board {grid = g} ((i, j), newCell) =
  b {grid = g V.// [(i, (g V.! i) V.// [(j, newCell)])]}