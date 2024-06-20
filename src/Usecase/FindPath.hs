module Usecase.FindPath where
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.SearchPattern
import Domain.TypeCheck.TypeError
import Domain.TypeCheck.Path
import Data.List (sortOn)

dfs :: Node -> ScopeGraph -> SearchPattern -> Path -> [Path]
dfs node scopeGraph ValUsage currentPath =
    if null currentPath then
        let possibleEdges = filter (\(Edge _ dest _) -> label dest == label node) (edges scopeGraph)
            uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
        in concatMap (\(Edge src dest et) -> dfs node scopeGraph ValUsage [PathComponent dest et src]) uEdges
    else
        let currentNode = toNode (last currentPath)
            possibleEdges = filter (\(Edge src dest et) -> (et == D && label src == label currentNode) ||
                                                           (et == P && label dest == label currentNode)) (edges scopeGraph)
        in concatMap (\(Edge src dest et) ->
                            let newComponent = if et == D then PathComponent src et dest else PathComponent dest et src
                                newPath = currentPath ++ [newComponent]
                            in if et == D && declNodeNameMatches dest (extractNodeName node)
                               then [newPath]
                               else if et == D
                                    then []
                                    else dfs node scopeGraph ValUsage newPath
                    ) possibleEdges
dfs _ _ FuncCall _ = []

findValidPath :: Node -> ScopeGraph -> SearchPattern -> Either TypeError Path
findValidPath startNode scopeGraph pattern =
    let paths = dfs startNode scopeGraph pattern []
        numberOfPaths = length paths
        possibleEdges = filter (\(Edge _ dest _) -> label dest == label startNode) (edges scopeGraph)
        uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
    in if numberOfPaths == 0
         then Left $ NotInScope (extractNodeName startNode) $ nodeInfo $ source $ head uEdges
         else if numberOfPaths == 1
                then Right $ head paths
                else 
                    let sortedPaths = sortOn length paths
                        listOfPathsLengths = map length sortedPaths
                        shortestPathLength = head listOfPathsLengths
                        shortestPaths = takeWhile (\path -> length path == shortestPathLength) sortedPaths
                    in if length shortestPaths == 1
                          then Right $ head shortestPaths
                          else Left $ MultipleDeclarations (extractNodeName startNode) (nodeInfo startNode)
