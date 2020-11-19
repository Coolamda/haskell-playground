module Lib
    ( someFunc
    , todoApp
    , numberGuesser
    ) where

import qualified Todo as Todo
import qualified NumberGuesser as NumberGuesser

someFunc :: IO ()
someFunc = putStrLn "someFunc"

todoApp :: IO ()
todoApp = Todo.main

numberGuesser :: IO ()
numberGuesser = NumberGuesser.main
