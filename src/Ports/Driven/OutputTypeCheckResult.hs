module Ports.Driven.OutputTypeCheckResult where
import Domain.ScopeGraph.ScopeGraph
import Usecase.FindPath (dfsScopeGraph)
import Domain.TypeCheck.SearchPattern

scopeGraphTypeCheck :: FilePath -> ScopeGraph -> IO ()
scopeGraphTypeCheck outputFilePath scopeGraph =
    writeFile outputFilePath $ show $ map (\n -> dfsScopeGraph n scopeGraph VarUsage) (extractUsageNodeInfos scopeGraph)
