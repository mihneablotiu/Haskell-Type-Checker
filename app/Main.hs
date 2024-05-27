{-# LANGUAGE OverloadedStrings #-}

import Ports.Driving.InputJSONFile
import Usecase.ConvertASTToScopeGraph
import Domain.ScopeGraph.ScopeGraph
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            result <- loadProgram filePath
            case result of
                Left err -> putStrLn err
                Right prog -> do
                    let scopeGraph = convertProgram prog
                    putStrLn $ scopeGraphToDot scopeGraph
        _ -> putStrLn "Usage: program <path-to-file>"
