module Main where

import Control.Monad
import Data.Maybe
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  void $ return window # set title "Example"
