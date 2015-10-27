module Main where

import App.Params
import App.Ui

main :: IO ()
main = getParams >>= runUi
