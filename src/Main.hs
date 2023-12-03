{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Board
  ( Board (state),
    BoardState (..),
    VisualBoard (grid),
    VisualState (..),
    generateBoard,
    getBoardVisuals,
    revealCell,
    toggleFlag,
  )
import Control.Monad
import Data.Bits
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (grid)
import System.Random (RandomGen, newStdGen)

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

  endText <- UI.div

  let setTextAndStyles s styles =
        element endText
          # set style styles
          # set UI.text s

  let modeButton = do
        mode <- liftIO $ readIORef modeRef
        let image m =
              [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                  # set UI.src (getImage m)
              ]
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ image mode
        on UI.click btn $ \_ -> do
          liftIO $ modifyIORef modeRef (\s -> if s == Flagging then Mining else Flagging)
          mode <- liftIO $ readIORef modeRef
          element btn # set children []
          element btn #+ image mode
        return btn
        where
          getImage m = path ++ "/" ++ name
            where
              name = (if m == Mining then "pickaxe" else "flag") ++ ".png"

  let updateVisuals board action = do
        element <- getElementById window "board"
        case element of
          Nothing -> pure ()
          Just x -> do
            pure x # set children [] -- Clear existing children
            let buttons = mkTable action modeRef $ grid $ getBoardVisuals board
            case state board of
              Won -> void $ setTextAndStyles "You Win!!" $ ("color", "green") : darkSoulsDeathStyle
              Lost -> void $ setTextAndStyles "You Lose" $ ("color", "red") : darkSoulsDeathStyle
              Playing -> pure ()

            void $ pure x #+ [buttons]

  let revealAndUpdateBoard :: (Int, Int) -> (Board -> (Int, Int) -> Board) -> UI ()
      revealAndUpdateBoard indices action = do
        currentBoard <- liftIO $ readIORef boardRef
        -- Update the board state by running the update action
        if state currentBoard == Playing
          then do
            let updatedBoard = action currentBoard indices
            -- Write the updated state back to the IORef
            liftIO $ writeIORef boardRef updatedBoard

            updateVisuals updatedBoard revealAndUpdateBoard
          else pure ()

  let buttons = mkTable revealAndUpdateBoard modeRef $ grid $ getBoardVisuals board

  let refreshButton = do
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                   # set UI.src (path ++ "/" ++ "refresh.png")
               ]
        on UI.click btn $ \_ -> getBody window # set children [] >> setup window

        return btn

  let homeButton = do
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                   # set UI.src (path ++ "/" ++ "home-button.png")
               ]
        on UI.click btn $ \_ -> liftIO $ print "Home button clicked!!!" -- TODO

        return btn

  void $
    getBody window
      #+ [ UI.div
             # set style [("width", "600px"), ("position", "absolute"), ("left", "50%"), ("top", "50%"), ("transform", "translate(-50%, -50%)")]
             #+ [ UI.div
                    # set style [("display", "flex"), ("flexDirection", "row"), ("justifyContent", "space-around")]
                    #+ [homeButton, refreshButton, modeButton], -- Evenly space buttons in this div
                  UI.div
                    # set style [("width", "100%"), ("height", "600px")]
                    #+ [buttons, element endText]
                ]
         ]

darkSoulsDeathStyle =
  [ ("background-color", "#222"),
    ("font-size", "3em"),
    ("text-align", "center"),
    ("width", "100%"),
    ("position", "fixed"),
    ("top", "50%"),
    ("left", "50%"),
    ("transform", "translate(-50%, -50%)")
  ]

mkCell actionOnClick modeRef (indices, s) = do
  btn <-
    UI.td
      # set style [("width", "auto"), ("height", "auto"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
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
  where
    boardAction m isClick = if (m == Mining) /= isClick then toggleFlag else revealCell

mkRow actionOnClick modeRef cols =
  UI.tr
    # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkCell actionOnClick modeRef) cols

mkTable actionOnClick modeRef rows =
  UI.table
    # set UI.id_ "board"
    # set style [("width", "100%"), ("height", "100%"), ("border-spacing", "0px"), ("border-collapse", "collapse"), ("cellspacing", "0px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkRow actionOnClick modeRef) rows