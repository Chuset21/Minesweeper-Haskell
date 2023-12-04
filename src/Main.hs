{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Agent (Move (..), makeMove, uncoverRandom)
import Board
  ( Board (state),
    BoardState (..),
    VisualBoard (VisualBoard, grid),
    VisualState (..),
    generateBoard,
    getBoardVisuals,
    revealCell,
    toggleFlag,
  )
import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( Config (jsStatic),
    MonadIO (liftIO),
    UI,
    Window,
    children,
    defaultConfig,
    element,
    ffi,
    getBody,
    getElementById,
    on,
    runFunction,
    runUI,
    set,
    startGUI,
    string,
    style,
    text,
    title,
    (#),
    (#+),
  )
import System.Random (RandomGen)

-- Type to know if we're in flagging mode or mining, I used this instead of a boolean for readability
data Mode = Flagging | Mining deriving (Eq, Show, Ord, Enum, Bounded)

-- The path to our images
path = "static"

getDiff :: VisualBoard -> VisualBoard -> [((Int, Int), VisualState)]
getDiff VisualBoard {grid = prev} VisualBoard {grid = new} = concat new \\ concat prev

changeDiffs window prev new =
  mapM_
    ( \(ind, s) -> do
        element <- getElementById window $ show ind
        case element of
          Nothing -> pure ()
          Just cell -> do
            img <- getCellImage s
            void $ pure cell # set children [img]
    )
    (getDiff prev new)

getCellImage :: VisualState -> UI UI.Element
getCellImage s = fromJust (Map.lookup s images) -- This should never throw

-- Cache the images
images :: Map.Map VisualState (UI UI.Element)
images =
  Map.fromList $
    [ (Covered, buildCellImage Covered),
      (Uncovered, buildCellImage Uncovered),
      (Flagged, buildCellImage Flagged),
      (Exploded, buildCellImage Exploded)
    ]
      ++ [(SurroundingMines n, buildCellImage $ SurroundingMines n) | n <- [1 .. 8]]

-- Build a cell image
buildCellImage s =
  UI.img
    # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
    # set UI.src (imageURL s)

-- Function to get image URL
imageURL :: VisualState -> String
imageURL = getImagePath . imageFileName

-- Go from VisualState to an image
imageFileName :: VisualState -> String
imageFileName Covered = "covered"
imageFileName Uncovered = "uncovered"
imageFileName (SurroundingMines n) = "number" ++ show n
imageFileName Flagged = "flag"
imageFileName Exploded = "exploded"

modeImageClassName = "modeImage"

boardId = "board"

cellClassName = "cell"

rowClassName = "row"

-- Delete all the board elements
deleteAllBoardElements :: Window -> UI ()
deleteAllBoardElements window = do
  mapM_ UI.delete =<< UI.getElementsByClassName window cellClassName
  mapM_ UI.delete =<< UI.getElementsByClassName window rowClassName

getModeImage :: Mode -> UI UI.Element
getModeImage m = fromJust (Map.lookup m modeImages) -- This should never throw

-- Cache mode images
modeImages :: Map.Map Mode (UI UI.Element)
modeImages = Map.fromList [(x, buildModeImage x) | x <- [minBound :: Mode .. maxBound]]

-- Build a mode image for the mode butoon
buildModeImage :: Mode -> UI UI.Element
buildModeImage m =
  UI.img
    # set UI.class_ modeImageClassName
    # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
    # set UI.src (getModeImageName m)

-- Get the mode image name
getModeImageName :: Mode -> String
getModeImageName m = getImagePath (if m == Mining then "pickaxe" else "flag")

-- Get image path
getImagePath :: String -> String
getImagePath imageName = path ++ "/" ++ imageName ++ ".png"

-- Image for refresh icon
refreshImage :: UI UI.Element
refreshImage =
  UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
    # set UI.src (getImagePath "refresh")

-- Image for home icon
homeImage :: UI UI.Element
homeImage =
  UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
    # set UI.src (getImagePath "home-button")

-- Image for auto move icon
autoMoveImage :: UI UI.Element
autoMoveImage =
  UI.img # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("display", "block")]
    # set UI.src (getImagePath "play")

main :: IO ()
main = do
  startGUI defaultConfig {jsStatic = Just $ path ++ "/."} setup

-- Main pages common button styles
mainPageButtonStyles =
  [ ("width", "260"),
    ("border", "solid black 2px"),
    ("font-size", "4em"),
    ("background", "grey"),
    ("border-radius", "5px")
  ]

-- Function used for remove all contenct from the page
resetAllChildren window = getBody window # set children []

-- Landing page to select your difficulty
setup :: Window -> UI ()
setup window = do
  resetAllChildren window
  getBody window # set style [("background-image", "url('https://www.newegg.com/insider/wp-content/uploads/2014/04/windows_xp_bliss-wide.jpg')")] -- Windows background
  return window # set title "Minesweeper"

  pageTitle <- UI.h1 # set text "Minesweeper" # set style [("text-align", "center"), ("font-size", "6em")]
  easyBtn <- UI.button #+ [string "Easy"] # set style (("margin-bottom", "40px") : mainPageButtonStyles)
  mediumBtn <- UI.button #+ [string "Medium"] # set style (("margin-bottom", "40px") : mainPageButtonStyles)
  hardBtn <- UI.button #+ [string "Hard"] # set style mainPageButtonStyles

  let homePage =
        [ element pageTitle,
          UI.div
            # set style [("position", "absolute"), ("left", "50%"), ("top", "50%"), ("transform", "translate(-50%, -50%)")]
            #+ [ UI.div
                   # set style [("display", "flex"), ("flexDirection", "column"), ("justifyContent", "space-around")]
                   #+ [element easyBtn, element mediumBtn, element hardBtn] -- Evenly space buttons in this div
               ]
        ]

  let deleteHomePage () = mapM_ UI.delete [pageTitle, easyBtn, mediumBtn, hardBtn]

  getBody window #+ homePage

  -- I went off of the difficulties seen online, the google game that shows up when you search for minesweeper
  on UI.click easyBtn $ \_ -> do
    deleteHomePage ()
    playMinesweeper 10 10 window

  on UI.click mediumBtn $ \_ -> do
    deleteHomePage ()
    playMinesweeper 18 40 window

  on UI.click hardBtn $ \_ -> do
    deleteHomePage ()
    playMinesweeper 24 99 window

-- Play the actual minesweeper game
playMinesweeper :: Int -> Int -> Window -> UI ()
playMinesweeper size numOfMines window = do
  resetAllChildren window
  runFunction $ ffi "document.addEventListener('contextmenu', function(event) {event.preventDefault();});" -- Disable the context menu popup on right clicks

  -- Create a mutable reference to the board
  boardRef <- liftIO $ newIORef =<< generateBoard size numOfMines
  board <- liftIO $ readIORef boardRef

  -- Create a mutable reference to the current mode
  modeRef <- liftIO $ newIORef Mining

  endText <- UI.div
  let setTextAndStyles s styles =
        element endText
          # set style styles
          # set UI.text s

  -- This functions handles winning or losing condition and showing appropriate text
  let showEndGameBanner state = do
        case state of
          Won -> void $ setTextAndStyles "You Win!!" $ ("color", "rgba(0, 255, 0, 0.8)") : darkSoulsDeathStyle
          Lost -> void $ setTextAndStyles "You Lose" $ ("color", "rgba(255, 0, 0, 0.8)") : darkSoulsDeathStyle
          Playing -> pure ()

  -- This function handles revealing and updating the board visuals
  let revealAndUpdateBoard :: (Int, Int) -> (Board -> (Int, Int) -> Board) -> UI ()
      revealAndUpdateBoard indices action = do
        prevBoard <- liftIO $ readIORef boardRef
        -- Update the board state by running the update action
        when (state prevBoard == Playing) $ do
          let updatedBoard = action prevBoard indices
          -- Write the updated state back to the IORef
          liftIO $ writeIORef boardRef updatedBoard

          -- Only change the differences
          changeDiffs window (getBoardVisuals prevBoard) (getBoardVisuals updatedBoard)

          showEndGameBanner $ state updatedBoard

  -- Button to start a new game in the current difficulty
  refreshButton <-
    UI.button
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
      #+ [refreshImage]

  -- Return to the home screen (setup function)
  homeButton <-
    UI.button
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
      #+ [homeImage]

  -- Button to get the AI to play a move
  autoMoveButton <-
    UI.button
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
      #+ [autoMoveImage]

  -- Button to display and change the current mode, mining or flagging
  modeButton <- do
    mode <- liftIO $ readIORef modeRef
    UI.button
      # set style [("width", "50px"), ("height", "50px"), ("margin", "0px"), ("padding", "0px")]
      #+ [getModeImage mode]

  -- Element representing the minesweeper board, as a table of buttons
  boardElement <- mkTable revealAndUpdateBoard modeRef $ grid $ getBoardVisuals board

  let minesweeperPage =
        [ UI.div
            # set style [("width", "600px"), ("position", "absolute"), ("left", "50%"), ("top", "50%"), ("transform", "translate(-50%, -50%)")]
            #+ [ UI.div
                   # set style [("display", "flex"), ("flexDirection", "row"), ("justifyContent", "space-around")]
                   #+ map element [homeButton, refreshButton, modeButton, autoMoveButton], -- Evenly space buttons in this div
                 UI.div
                   # set style [("width", "100%"), ("height", "600px")]
                   #+ map element [boardElement, endText]
               ]
        ]

  let deleteMinesweeperPage () = do
        deleteAllBoardElements window
        mapM_ UI.delete [modeButton, homeButton, refreshButton, autoMoveButton, boardElement, endText]
        minesweeperPageElements <- liftIO $ mapM (runUI window) minesweeperPage
        mapM_ UI.delete minesweeperPageElements
  getBody window #+ minesweeperPage

  -- Click handlers
  on UI.click modeButton $ \_ -> do
    liftIO $ modifyIORef modeRef (\s -> if s == Flagging then Mining else Flagging)
    mode <- liftIO $ readIORef modeRef
    modeImg <- getModeImage mode
    element modeButton # set children [modeImg]

  on UI.click homeButton $ \_ -> do
    deleteMinesweeperPage ()
    setup window
  on UI.click refreshButton $ \_ -> do
    prev <- liftIO $ readIORef boardRef
    new <- liftIO $ generateBoard size numOfMines
    liftIO $ writeIORef boardRef new
    setTextAndStyles "" []
    changeDiffs window (getBoardVisuals prev) (getBoardVisuals new)

  on UI.click autoMoveButton $ \_ -> do
    currentBoard <- liftIO $ readIORef boardRef
    when (state currentBoard == Playing) $ do
      case makeMove $ getBoardVisuals currentBoard of
        Just (Uncover ind) -> revealAndUpdateBoard ind revealCell
        Just (Flag ind) -> revealAndUpdateBoard ind toggleFlag
        Nothing -> do
          x <- liftIO $ uncoverRandom $ getBoardVisuals currentBoard
          revealAndUpdateBoard x revealCell

-- A styling to have text be in the sort of "You Died" dark souls death style, even if you win...
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

-- Make an individual cell, with a callback
mkCell :: ((Int, Int) -> (Board -> (Int, Int) -> Board) -> UI void) -> IORef Mode -> ((Int, Int), VisualState) -> UI UI.Element
mkCell actionOnClick modeRef (indices, s) = do
  btn <-
    UI.td
      # set UI.class_ cellClassName
      # set UI.id_ (show indices)
      # set style [("width", "auto"), ("height", "auto"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
      #+ [getCellImage s]
  on UI.click btn $ \_ -> getAction True
  on UI.contextmenu btn $ \_ -> getAction False

  return btn
  where
    -- Left click mines, right click flags if on mining mode, otherwise swap them around
    boardAction m isClick = if (m == Mining) /= isClick then toggleFlag else revealCell
    getAction isClick = do
      mode <- liftIO $ readIORef modeRef
      actionOnClick indices (boardAction mode isClick)

-- Make a row of cells
mkRow :: ((Int, Int) -> (Board -> (Int, Int) -> Board) -> UI void) -> IORef Mode -> [((Int, Int), VisualState)] -> UI UI.Element
mkRow actionOnClick modeRef cols =
  UI.tr
    # set UI.class_ rowClassName
    # set style [("width", "100%"), ("height", "100%"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkCell actionOnClick modeRef) cols

-- Make a table of cells
mkTable :: ((Int, Int) -> (Board -> (Int, Int) -> Board) -> UI void) -> IORef Mode -> [[((Int, Int), VisualState)]] -> UI UI.Element
mkTable actionOnClick modeRef rows =
  UI.table
    # set UI.id_ "board"
    # set style [("width", "100%"), ("height", "100%"), ("border-spacing", "0px"), ("border-collapse", "collapse"), ("cellspacing", "0px"), ("margin", "0px"), ("padding", "0px"), ("border", "1px solid black")]
    #+ map (mkRow actionOnClick modeRef) rows