module Usecase.FindPath where
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.SearchPattern
import Domain.TypeCheck.Path
import Domain.TypeCheck.TypeError

dfsHelper :: Node -> ScopeGraph -> [Path] -> [Path]
dfsHelper node scopeGraph visited = 
    if null visited then
        let possibleEdges = filter (\(Edge _ dest et) -> et == U && (label node == label dest)) (edges scopeGraph)
        in map (\(Edge src dest et) -> [PathComponent (nodeInfo dest) et (nodeInfo src)]) possibleEdges
    else []

dfsScopeGraph :: Node -> ScopeGraph -> SearchPattern -> Either TypeError Path
dfsScopeGraph node scopeGraph pattern =
    case pattern of
        VarUsage -> 
            let visited = []
                possiblePaths = dfsHelper node scopeGraph visited
            in 
                if null possiblePaths then
                    Left $ UnexpectedError "dfsScopeGraph: No path found."
                else
                    Right $ head possiblePaths
        FuncCall -> undefined
