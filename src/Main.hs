{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Board
import System.Random (newStdGen, RandomGen)
import Control.Monad.RWS (MonadState(put))

main :: IO ()
main = do
  gen <- newStdGen
  -- repeatProcess gen 100 25 99
  let (b, _) = generateSeededBoard gen 10 45
  putStrLn $ prettyPrintBoardWithNumbers b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoardWithNumbers b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoardWithNumbers b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoardWithNumbers b

  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Example"
