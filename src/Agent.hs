module Agent (Move (..), makeMove, uncoverRandom) where

import Board
  ( BoardState (..),
    Boarded (..),
    VisualBoard (..),
    VisualState (..),
  )
import Control.Monad.Random (MonadRandom (getRandomR))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)

data Move = Uncover !(Int, Int) | Flag !(Int, Int)

getSquareSafe :: VisualBoard -> (Int, Int) -> Maybe ((Int, Int), VisualState)
getSquareSafe b@VisualBoard {grid = g} ind@(i, j) =
  if inBounds b ind
    then Just ((g !! i) !! j)
    else Nothing

getSquareUnsafe :: VisualBoard -> (Int, Int) -> ((Int, Int), VisualState)
getSquareUnsafe b@VisualBoard {grid = g} (i, j) = (g !! i) !! j

getNeighbours :: VisualBoard -> (Int, Int) -> [((Int, Int), VisualState)]
getNeighbours b (i, j) =
  let neighbourIndices =
        [ (x, y) | x <- [i -1 .. i + 1], y <- [j -1 .. j + 1], not $ (x == i) && (y == j), inBounds b (x, y)
        ]
   in map (getSquareUnsafe b) neighbourIndices

getIf :: [[t]] -> (t -> Bool) -> [t]
getIf g predicate = [t | row <- g, t <- row, predicate t]

uncoverRandom :: MonadRandom m => VisualBoard -> m (Int, Int)
uncoverRandom b@VisualBoard {state = s, grid = g} =
  if null coveredIndices
    then pure (0, 0) -- This should never happen, since this shouldn't be called when the game state isn't 'Playing' or if all the remaining cells are flagged (some must be flagged wrong, but the AI did not do this)
    else do
      i <- getRandomR (0, length coveredIndices - 1)
      pure (coveredIndices !! i)
  where
    coveredIndices = map fst $ getIf g (\(_, vs) -> vs == Covered)
    maxIndex = length coveredIndices - 1

makeMove :: VisualBoard -> Maybe Move
makeMove b@VisualBoard {state = s, grid = g} =
  if s /= Playing
    then Nothing
    else
      let cellsWithSurroundingMines = getIf g (\(_, vs) -> case vs of SurroundingMines _ -> True; _ -> False) -- First get all the uncovered squares that have surrounding mines
       in fromMaybe Nothing $ find isJust (map (tryPlayObvious b . fst) cellsWithSurroundingMines) -- Map to get only the indices, then map to do tryPlayObvious

-- If the current square has the same number of surrounding mines and flags then the rest of the squares must be safe to mine
-- If the current sqaure has the same number of surrounding mines squares as covered cells + flagged cells then they are all mines, we can flag them
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

moveOnFirst :: (t -> a) -> [(t, b)] -> Maybe a
moveOnFirst _ [] = Nothing
moveOnFirst m (first : _) = Just (m (fst first))

uncoverFirst :: [((Int, Int), b)] -> Maybe Move
uncoverFirst = moveOnFirst Uncover

flagFirst :: [((Int, Int), b)] -> Maybe Move
flagFirst = moveOnFirst Flag
