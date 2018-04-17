module Main where

import System.Environment
import Lib

import Parser

import Text.Show.Pretty hiding (List, Value, Float)

main :: IO ()
main = do
    cmdArgs <- getArgs
    case cmdArgs of
        []  -> putStrLn "Insert a string"
        x:_ -> do
            x' <- readFile x
            let erg = parseStr x'
            putStrLn $ ppShow erg
