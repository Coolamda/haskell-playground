{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo where

import Data.Aeson
import Data.List
import System.Environment
import System.IO
import System.Directory

type Args = [String]
type Todo = String
type TodoList = [Todo]
type Command = Args -> IO ()

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("view", viewCommand)
           , ("add", addCommand)
           , ("remove", removeCommand)
           , ("edit", editCommand)
           , ("bump", bumpCommand)
           ]


-- COMMANDS
viewCommand :: Args -> IO ()
viewCommand  [fileName] = do
    content <- readFile fileName
    putStr . viewTodos $ lines content

addCommand :: Args -> IO ()
addCommand [fileName, input] = do
    content <- readFile fileName
    let todos = lines content
        newTodoList = addTodo todos input
    saveToFile fileName $ unlines newTodoList

removeCommand :: Args -> IO ()
removeCommand [fileName, number] = do
    content <- readFile fileName
    let todos = lines content
        index = read number
        newTodoList = removeTodo todos index
    saveToFile fileName $ unlines newTodoList

editCommand :: Args -> IO ()
editCommand [fileName, number, input] = do
    content <- readFile fileName
    let index = read number
        todoItems = lines content
        newTodoItems = editTodo todoItems index input
    saveToFile fileName $ unlines newTodoItems

bumpCommand :: Args -> IO ()
bumpCommand [fileName, number] = do
    content <- readFile fileName
    let todos = lines content
        index = read number
        newTodoItems = bumpTodo todos index
    saveToFile fileName $ unlines newTodoItems


-- TODO FUNCTIONS
viewTodos :: TodoList -> String
viewTodos = unlines . zipWith (\number todo -> show number ++ " - " ++ todo) [0..]

addTodo :: TodoList -> Todo -> TodoList
addTodo todoList todo = reverse (todo : reverse todoList)

removeTodo :: TodoList -> Int -> TodoList
removeTodo todoList index = delete (todoList !! index) todoList

editTodo :: TodoList -> Int -> Todo -> TodoList
editTodo todoList index todo = replaceAtN todoList index todo

bumpTodo :: TodoList -> Int -> TodoList
bumpTodo todoList index = let todo = todoList !! index
                          in  todo : (delete todo todoList)

-- HELPERS
replaceAtN ::  [a] -> Int -> a -> [a]
replaceAtN xs index input = let (before, _:after) = splitAt index xs
                            in  before ++ [input] ++ after

saveToFile :: FilePath -> String -> IO ()
saveToFile fileName str = do
    (tempName, tempHandle) <- openTempFile "." "temp.txt"
    hPutStr tempHandle str
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
