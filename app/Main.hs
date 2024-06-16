{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Ports.Driving.InputJSONFile
import Usecase.ConvertProgramToScopeGraph
import Ports.Driven.OutputDOTGraph
import Ports.Driven.OutputTypeCheckResult

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFilePath, outputFilePath] -> do
            result <- loadProgram inputFilePath
            case result of
                Left err -> putStrLn err
                Right prog -> do
                    let scopeGraph = convertProgram prog
                    
                    scopeGraphToDot scopeGraph
                    scopeGraphTypeCheck outputFilePath scopeGraph prog
        _ -> putStrLn "Usage: program <path-to-file>"
