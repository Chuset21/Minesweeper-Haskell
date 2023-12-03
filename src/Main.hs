{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Agent (Move (..), makeMove, uncoverRandom)
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

imageURL :: VisualState -> String
imageURL s = path ++ "/" ++ imageFileName s

-- Go from VisualState to an image
imageFileName :: VisualState -> String
imageFileName Covered = "covered.png"
imageFileName Uncovered = "uncovered.png"
imageFileName (SurroundingMines n) = "number" ++ show n ++ ".png"
imageFileName Flagged = "flag.png"
imageFileName Exploded = "exploded.png"

main :: IO ()
main = do
  startGUI defaultConfig {jsStatic = Just $ path ++ "/."} setup

mainPageButtonStyles =
  [ ("width", "260"),
    ("border", "solid black 2px"),
    ("font-size", "4em"),
    ("background", "grey"),
    ("border-radius", "5px")
  ]

deleteAll window = getBody window # set children []

setup :: Window -> UI ()
setup window = do
  getBody window # set style [("background-image", "url('https://www.newegg.com/insider/wp-content/uploads/2014/04/windows_xp_bliss-wide.jpg')")] -- Windows background
  return window # set title "Minesweeper"

  pageTitle <- UI.h1 # set text "Minesweeper" # set style [("text-align", "center"), ("font-size", "6em")]
  easyBtn <- UI.button #+ [string "Easy"] # set style (("margin-bottom", "40px") : mainPageButtonStyles)
  mediumBtn <- UI.button #+ [string "Medium"] # set style (("margin-bottom", "40px") : mainPageButtonStyles)
  hardBtn <- UI.button #+ [string "Hard"] # set style mainPageButtonStyles

  getBody window
    #+ [ element pageTitle,
         UI.div
           # set style [("position", "absolute"), ("left", "50%"), ("top", "50%"), ("transform", "translate(-50%, -50%)")]
           #+ [ UI.div
                  # set style [("display", "flex"), ("flexDirection", "column"), ("justifyContent", "space-around")]
                  #+ [element easyBtn, element mediumBtn, element hardBtn] -- Evenly space buttons in this div
              ]
       ]

  on UI.click easyBtn $ \_ -> do
    deleteAll window
    playMinesweeper 10 10 window

  on UI.click mediumBtn $ \_ -> do
    deleteAll window
    playMinesweeper 18 40 window

  on UI.click hardBtn $ \_ -> do
    deleteAll window
    playMinesweeper 24 99 window

playMinesweeper :: Int -> Int -> Window -> UI ()
playMinesweeper size numOfMines window = do
  runFunction $ ffi "document.addEventListener('contextmenu', function(event) {event.preventDefault();});" -- Disable the context menu popup on right clicks

  -- Create a mutable reference to the board
  boardRef <- liftIO $ newIORef =<< generateBoard size numOfMines
  board <- liftIO $ readIORef boardRef

  modeRef <- liftIO $ newIORef Mining

  endText <- UI.div

  let setTextAndStyles s styles =
        element endText
          # set style styles
          # set UI.text s

  let modeButton = do
        mode <- liftIO $ readIORef modeRef
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ buildImage mode
        on UI.click btn $ \_ -> do
          liftIO $ modifyIORef modeRef (\s -> if s == Flagging then Mining else Flagging)
          mode' <- liftIO $ readIORef modeRef
          elements <- liftIO $ mapM (runUI window) (buildImage mode')
          element btn # set children elements
        return btn
        where
          buildImage m =
            [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                # set UI.src (getImage m)
            ]
          getImage m = path ++ "/" ++ ((if m == Mining then "pickaxe" else "flag") ++ ".png")

  -- This function handles updating the board visuals
  let updateVisuals board action = do
        element <- getElementById window "board"
        case element of
          Nothing -> pure ()
          Just x -> do
            buttons <- mkTable action modeRef $ grid $ getBoardVisuals board
            case state board of
              Won -> void $ setTextAndStyles "You Win!!" $ ("color", "rgba(0, 255, 0, 0.8)") : darkSoulsDeathStyle
              Lost -> void $ setTextAndStyles "You Lose" $ ("color", "rgba(255, 0, 0, 0.8)") : darkSoulsDeathStyle
              Playing -> pure ()

            void $ pure x # set children [buttons] -- Set new board

  -- This function handles revealing and updating the board visuals
  let revealAndUpdateBoard :: (Int, Int) -> (Board -> (Int, Int) -> Board) -> UI ()
      revealAndUpdateBoard indices action = do
        currentBoard <- liftIO $ readIORef boardRef
        -- Update the board state by running the update action
        when (state currentBoard == Playing) $ do
          let updatedBoard = action currentBoard indices
          -- Write the updated state back to the IORef
          liftIO $ writeIORef boardRef updatedBoard

          updateVisuals updatedBoard revealAndUpdateBoard

  let buttons = mkTable revealAndUpdateBoard modeRef $ grid $ getBoardVisuals board

  let refreshButton = do
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                   # set UI.src (path ++ "/" ++ "refresh.png")
               ]
        on UI.click btn $ \_ -> deleteAll window >> playMinesweeper size numOfMines window

        return btn

  let homeButton = do
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                   # set UI.src (path ++ "/" ++ "home-button.png")
               ]
        on UI.click btn $ \_ -> deleteAll window >> setup window

        return btn

  let autoMoveButton = do
        btn <-
          UI.button
            # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
            #+ [ UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
                   # set UI.src (path ++ "/" ++ "play.png")
               ]
        on UI.click btn $ \_ -> do
          currentBoard <- liftIO $ readIORef boardRef
          when (state currentBoard == Playing) $ do
            let visualB = getBoardVisuals currentBoard
             in case makeMove $ getBoardVisuals currentBoard of
                  Just (Uncover ind) -> revealAndUpdateBoard ind revealCell
                  Just (Flag ind) -> revealAndUpdateBoard ind toggleFlag
                  Nothing -> do
                    x <- liftIO $ uncoverRandom visualB
                    revealAndUpdateBoard x revealCell
        return btn

  void $
    getBody window
      #+ [ UI.div
             # set style [("width", "600px"), ("position", "absolute"), ("left", "50%"), ("top", "50%"), ("transform", "translate(-50%, -50%)")]
             #+ [ UI.div
                    # set style [("display", "flex"), ("flexDirection", "row"), ("justifyContent", "space-around")]
                    #+ [homeButton, refreshButton, modeButton, autoMoveButton], -- Evenly space buttons in this div
                  UI.div
                    # set style [("width", "100%"), ("height", "600px")]
                    #+ [buttons, element endText]
                ]
         ]

darkSoulsDeathStyle =
  [ ("background-color", "rgba(34, 34, 34, 0.8)"),
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