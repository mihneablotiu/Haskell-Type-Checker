module Ports.Driving.InputJSONFile where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import Domain.Language.LanguageComponents

loadProgram :: FilePath -> IO (Either String Program)
loadProgram filePath = do
    content <- B.readFile filePath
    let program = eitherDecode content :: Either String Program
    return $ case program of
        Left err -> Left $ "Failed to parse program: " ++ err
        _ -> program
