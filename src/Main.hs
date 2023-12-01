{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Board
import System.Random (newStdGen, RandomGen)
import Control.Monad.RWS (MonadState(put))

repeatProcess :: (Eq n, Num n, RandomGen g) => g -> n -> Int -> Int -> IO ()
repeatProcess _ 0 _ _ = return ()
repeatProcess gen n size numBombs = do
  let (board, newGen) = generateSeededBoard gen size numBombs
  when (getTotalMines board /= numBombs) $ print "Error!!!!!!!!!!!!!!!!"
  repeatProcess newGen (n - 1) size numBombs

main :: IO ()
main = do
  gen <- newStdGen
  -- repeatProcess gen 100 25 99
  let (b, _) = generateSeededBoard gen 10 45
  putStrLn $ prettyPrintBoard b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoard b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoard b
  b <- generateBoard 10 45
  putStrLn $ prettyPrintBoard b

  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Example"
