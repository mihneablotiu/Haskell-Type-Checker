module Ports.Driven.OutputTypeCheckResult where
import Domain.ScopeGraph.ScopeGraph
import Domain.Language.LanguageComponents (Program)
import Usecase.TypeCheck (typeCheckProgram)

scopeGraphTypeCheck :: FilePath -> ScopeGraph -> Program -> IO ()
scopeGraphTypeCheck outputFilePath scopeGraph program =
    writeFile outputFilePath $ show $ typeCheckProgram program scopeGraph 
