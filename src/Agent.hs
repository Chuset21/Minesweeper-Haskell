module Agent (Move (..), makeMove, uncoverRandom) where

import Board
  ( BoardState (..),
    VisualBoard (..),
    VisualState (..),
    Boarded (size),
    inBounds,
  )
import Control.Monad.Random (MonadRandom (getRandomR))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)

-- A type of move, including the index to play it at
data Move = Uncover !(Int, Int) | Flag !(Int, Int)

-- Get a square unsafely
getSquareUnsafe :: VisualBoard -> (Int, Int) -> ((Int, Int), VisualState)
getSquareUnsafe b@VisualBoard {grid = g} (i, j) = (g !! i) !! j

-- Get the neighbours of a cell
getNeighbours :: VisualBoard -> (Int, Int) -> [((Int, Int), VisualState)]
getNeighbours b (i, j) =
  let neighbourIndices =
        [ (x, y) | x <- [i -1 .. i + 1], y <- [j -1 .. j + 1], not $ (x == i) && (y == j), inBounds b (x, y)
        ]
   in map (getSquareUnsafe b) neighbourIndices

-- Get a list of indexed cells that satisfy a condition
getIf :: [[t]] -> (t -> Bool) -> [t]
getIf g predicate = filter predicate $ concat g

-- Uncover a random square
uncoverRandom :: MonadRandom m => VisualBoard -> m (Int, Int)
uncoverRandom b@VisualBoard {grid = g} =
  if isFirstMove || null coveredIndices -- If it's the first move, then go for the top right corner, since it's best to go for the corners at the start, and we know that the cell will most likely be moved to 0,0 if the move was unsafe, like in the original minesweeper
    then pure (0, size b - 1) -- This should never happen outside the first move, since this shouldn't be called when the game state isn't 'Playing' or if all the remaining cells are flagged (some must be flagged wrong, but the AI did not do this)
    else do
      i <- getRandomR (0, length coveredIndices - 1)
      pure (coveredIndices !! i)
  where
    coveredIndices = map fst $ getIf g (\(_, vs) -> vs == Covered)
    isFirstMove = null $ getIf g (\(_, vs) -> case vs of SurroundingMines _ -> True; Uncovered -> True; _ -> False)

-- Make a move, by the strategy of tryPlayObvious, if there is no obvious move, return Nothing
makeMove :: VisualBoard -> Maybe Move
makeMove b@VisualBoard {state = s, grid = g} =
  if s /= Playing
    then Nothing
    else
      let cellsWithSurroundingMines = getIf g (\(_, vs) -> case vs of SurroundingMines _ -> True; _ -> False) -- First get all the uncovered squares that have surrounding mines
       in fromMaybe Nothing $ find isJust (map (tryPlayObvious b . fst) cellsWithSurroundingMines) -- Map to get only the indices, then map to do tryPlayObvious, get the first valid move, or nothing if no such move exists

-- If the current square has the same number of surrounding mines and flags then the rest of the squares must be safe to mine
-- If the current square has the same number of surrounding mines as covered cells + flagged cells then they are all mines, we can flag them
tryPlayObvious :: VisualBoard -> (Int, Int) -> Maybe Move
tryPlayObvious b@VisualBoard {state = s, grid = g} ind@(i, j) =
  case snd $ getSquareUnsafe b ind of
    -- We can only do this with squares that are mined and have at least one surrounding mine
    SurroundingMines n ->
      let neighbours = getNeighbours b ind
          coveredNeighbours = filter (\(i, s) -> s == Covered) neighbours
          surroundingFlagsNum = length $ filter (\(i, s) -> s == Flagged) neighbours
       in if surroundingFlagsNum == n
            then uncoverFirst coveredNeighbours -- Can only uncover a covered square
            else
              if length coveredNeighbours + surroundingFlagsNum == n
                then flagFirst coveredNeighbours
                else Nothing
    _ -> Nothing

-- Auxiliary function to make the first move in a list, or Nothing if it's empty, takes in the type of move and a list of indices and cell in our case (we only care about the indices)
moveOnFirst :: (t -> a) -> [(t, b)] -> Maybe a
moveOnFirst _ [] = Nothing
moveOnFirst m (first : _) = Just (m (fst first))

-- Uncover the first, using moveOnFirst
uncoverFirst :: [((Int, Int), b)] -> Maybe Move
uncoverFirst = moveOnFirst Uncover

-- Flag the first, using moveOnFirst
flagFirst :: [((Int, Int), b)] -> Maybe Move
flagFirst = moveOnFirst Flag
