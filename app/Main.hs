{-# LANGUAGE OverloadedStrings #-}

import Domain.Language.LanguageComponents
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            content <- B.readFile filePath
            let program = eitherDecode content :: Either String Program
            case program of
                Left err -> putStrLn $ "Failed to parse program: " ++ err
                Right prog -> print prog
        _ -> putStrLn "Usage: program <path-to-file>"
