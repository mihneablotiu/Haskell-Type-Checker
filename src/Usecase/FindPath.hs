module Usecase.FindPath where
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.SearchPattern
import Domain.TypeCheck.Path
import Data.Char (isDigit)

nodeNameMatches :: Node -> String -> Bool
nodeNameMatches (Node _ (DeclNode "Number" _)) targetName = length targetName == length (filter id $ map isDigit targetName)
nodeNameMatches (Node _ (DeclNode name _)) targetName = name == targetName
nodeNameMatches _ _ = False

extractNodeName :: Node -> String
extractNodeName (Node _ (UsageNode name)) = name
extractNodeName _ = error "Node does not have a name"

dfs :: Node -> ScopeGraph -> SearchPattern -> Path -> [Path]
dfs node scopeGraph VarUsage currentPath =
    if null currentPath then
        let possibleEdges = filter (\(Edge _ dest _) -> label dest == label node) (edges scopeGraph)
            uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
        in concatMap (\(Edge src dest et) -> dfs node scopeGraph VarUsage [PathComponent dest et src]) uEdges
    else
        let currentNode = toNode (last currentPath)
            possibleEdges = filter (\(Edge src dest et) -> (et == D && label src == label currentNode) ||
                                                           (et == P && label dest == label currentNode)) (edges scopeGraph)
        in concatMap (\(Edge src dest et) ->
                            let newComponent = if et == D then PathComponent src et dest else PathComponent dest et src
                                newPath = currentPath ++ [newComponent]
                            in if et == D && nodeNameMatches dest (extractNodeName node)
                               then [newPath]
                               else if et == D
                                    then []
                                    else dfs node scopeGraph VarUsage newPath
                    ) possibleEdges
dfs _ _ FuncCall _ = []

findAllValidPaths :: Node -> ScopeGraph -> SearchPattern -> [Path]
findAllValidPaths startNode scopeGraph pattern = dfs startNode scopeGraph pattern []