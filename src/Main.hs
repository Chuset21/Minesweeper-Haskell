{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board
import Control.Monad
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid)
import System.Random (RandomGen, newStdGen)

path = "static"

main :: IO ()
main = do
  startGUI defaultConfig {jsStatic = Just path} setup

imageURL :: VisualState -> String
imageURL s = path ++ "/" ++ imageFileName s

-- Go from VisualState to an image
imageFileName :: VisualState -> String
imageFileName Covered = "covered.png"
imageFileName Uncovered = "uncovered.png"
imageFileName (SurroundingMines n) = "number" ++ show n ++ ".png"
imageFileName Flagged = "flag.png"
imageFileName Exploded = "exploded.png"

setup :: Window -> UI ()
setup window = do
  return window # set title "Minesweeper"

  -- Create a mutable reference to the board
  boardRef <- liftIO $ newIORef =<< generateBoard 10 10

  board <- liftIO $ readIORef boardRef
  let visualBoard = getBoardVisuals board

  let updateVisuals board action = do
        element <- getElementById window "board"
        case element of
          Nothing -> pure ()
          Just x -> do
            pure x # set children [] -- Clear existing children
            let buttons = mkTable action $ grid $ getBoardVisuals board
            void $ pure x #+ [buttons]

  let revealAndUpdateBoard :: (Int, Int) -> (Board -> (Int, Int) -> Board) -> UI ()
      revealAndUpdateBoard indices action = do
        currentBoard <- liftIO $ readIORef boardRef
        -- Update the board state by revealing the clicked cell
        let updatedBoard = action currentBoard indices
        -- Write the updated state back to the IORef
        liftIO $ writeIORef boardRef updatedBoard

        updateVisuals updatedBoard revealAndUpdateBoard -- This is bugged and hangs the program
  let buttons = mkTable revealAndUpdateBoard $ grid visualBoard

  -- Add buttons to the body of the HTML document
  void $ getBody window #+ [buttons]

mkCell actionOnClick (indices, s) = do
  btn <- UI.td 
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")] 
      #+ [UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")] 
      # set UI.src (imageURL s)]
  on UI.click btn $ \_ -> actionOnClick indices revealCell

  return btn

mkRow actionOnClick cols =
  UI.tr
    # set style [("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkCell actionOnClick) cols

mkTable :: (t -> (Board -> (Int, Int) -> Board) -> UI void) -> [[(t, VisualState)]] -> UI Element
mkTable actionOnClick rows =
  UI.table
    # set UI.id_ "board"
    # set style [("border-spacing", "0px"), ("border-collapse", "collapse"), ("cellspacing", "0px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkRow actionOnClick) rows