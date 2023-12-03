{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board
import Control.Monad
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid)
import System.Random (RandomGen, newStdGen)
import Data.Bits

data Mode = Flagging | Mining deriving (Eq, Show)

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
  runFunction $ ffi "document.addEventListener('contextmenu', function(event) {event.preventDefault();});" -- Disable the context menu popup on right clicks

  -- Create a mutable reference to the board
  boardRef <- liftIO $ newIORef =<< generateBoard 10 10
  board <- liftIO $ readIORef boardRef

  modeRef <- liftIO $ newIORef Mining
  let modeButton = do
        mode <- liftIO $ readIORef modeRef
        btn <-
          UI.button # set text (btnText mode)
        on UI.click btn $ \_ -> do
          liftIO $ modifyIORef modeRef (\s -> if s == Flagging then Mining else Flagging)
          mode <- liftIO $ readIORef modeRef
          element btn # set text (btnText mode)
        return btn
        where btnText m = "Current mode: " ++ show m

  let updateVisuals board action = do
        element <- getElementById window "board"
        case element of
          Nothing -> pure ()
          Just x -> do
            pure x # set children [] -- Clear existing children
            let buttons = mkTable action modeRef $ grid $ getBoardVisuals board
            void $ pure x #+ [buttons]

  let revealAndUpdateBoard :: (Int, Int) -> (Board -> (Int, Int) -> Board) -> UI ()
      revealAndUpdateBoard indices action = do
        currentBoard <- liftIO $ readIORef boardRef
        -- Update the board state by running the update action
        let updatedBoard = action currentBoard indices
        -- Write the updated state back to the IORef
        liftIO $ writeIORef boardRef updatedBoard

        updateVisuals updatedBoard revealAndUpdateBoard -- This is bugged and hangs the program
  let buttons = mkTable revealAndUpdateBoard modeRef $ grid $ getBoardVisuals board

  let refreshButton = do
          btn <- UI.button 
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
             # set UI.src (path ++ "/" ++ "refresh.png")
              ]
          on UI.click btn $ \_ -> getBody window # set children [] >> setup window

          return btn

  -- Add buttons to the body of the HTML document
  void $ getBody window #+ [refreshButton, modeButton, buttons]

mkCell actionOnClick modeRef (indices, s) = do
  btn <-
    UI.td
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
      #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
             # set UI.src (imageURL s)
         ]
  on UI.click btn $ \_ -> do
    mode <- liftIO $ readIORef modeRef
    actionOnClick indices (boardAction mode True)
  on UI.contextmenu btn $ \_ -> do 
    mode <- liftIO $ readIORef modeRef
    actionOnClick indices (boardAction mode False)

  return btn
  where boardAction m isClick = if (m == Mining) /= isClick then toggleFlag else revealCell

mkRow actionOnClick modeRef cols =
  UI.tr
    # set style [("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkCell actionOnClick modeRef) cols

mkTable actionOnClick modeRef rows =
  UI.table
    # set UI.id_ "board"
    # set style [("border-spacing", "0px"), ("border-collapse", "collapse"), ("cellspacing", "0px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkRow actionOnClick modeRef) rows