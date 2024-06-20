module Ports.Driven.OutputTypeCheckResult where
import Usecase.TypeCheck (TypeCheckResult)

scopeGraphTypeCheck :: FilePath -> TypeCheckResult -> IO ()
scopeGraphTypeCheck outputFilePath typeCheckResult = writeFile outputFilePath $ show typeCheckResult
