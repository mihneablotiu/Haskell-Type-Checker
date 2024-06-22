module Usecase.FindPath where
import Domain.ScopeGraph.ScopeGraph
import Domain.TypeCheck.SearchPattern
import Domain.TypeCheck.TypeError
import Domain.TypeCheck.Path
import Data.List (sortOn)

dfs :: Node -> ScopeGraph -> SearchPattern -> Path -> [Path]
dfs node scopeGraph Reference currentPath =
    if null currentPath then
        let possibleEdges = filter (\(Edge _ dest _) -> label dest == label node) (edges scopeGraph)
            uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
        in concatMap (\(Edge src dest et) -> dfs node scopeGraph Reference [PathComponent dest et src]) uEdges
    else
        let currentNode = toNode (last currentPath)
            possibleEdges = filter (\(Edge src dest et) -> (et == D && label src == label currentNode) ||
                                                           (et == P && label dest == label currentNode && edgeT (last currentPath) /= Eq) ||
                                                           (et == TC && label src == label currentNode && (edgeT (last currentPath) == P || edgeT (last currentPath) == U)) ||
                                                           (et == Eq && label src == label currentNode && edgeT (last currentPath) == TC)) (edges scopeGraph)
        in concatMap (\(Edge src dest et) ->
                            let newComponent = if et == D || et == TC || et == Eq then PathComponent src et dest else PathComponent dest et src
                                newPath = currentPath ++ [newComponent]
                            in if et == D && declNodeNameMatches dest (extractNodeName node)
                                then [newPath]
                                else if et == D
                                    then []
                                    else dfs node scopeGraph Reference newPath
                    ) possibleEdges

dfs node scopeGraph (InstanceUsage tcName tcValue) currentPath =
    if null currentPath then
        let possibleEdges = filter (\(Edge _ dest _) -> label dest == label node) (edges scopeGraph)
            uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
        in concatMap (\(Edge src dest et) -> dfs node scopeGraph (InstanceUsage tcName tcValue) [PathComponent dest et src]) uEdges
    else
        let currentNode = toNode (last currentPath)
            possibleEdges = filter (\(Edge src dest et) -> (et == I && label src == label currentNode) ||
                                                           (et == P && label dest == label currentNode)) (edges scopeGraph)
        in concatMap (\(Edge src dest et) ->
                            let newComponent = if et == I then PathComponent src et dest else PathComponent dest et src
                                newPath = currentPath ++ [newComponent]
                            in if et == I && show (extractClassFromInstance dest) == show tcName && show (extractActualTypeFromInstance dest) == show tcValue
                               then [newPath]
                               else if et == I
                                    then []
                                    else dfs node scopeGraph (InstanceUsage tcName tcValue) newPath
                    ) possibleEdges

findValidPath :: Node -> ScopeGraph -> SearchPattern -> Either TypeError (Path, Bool)
findValidPath startNode scopeGraph pattern =
    let paths = dfs startNode scopeGraph pattern []
        numberOfPaths = length paths
        possibleEdges = filter (\(Edge _ dest _) -> label dest == label startNode) (edges scopeGraph)
        uEdges = filter (\(Edge _ _ et) -> et == U) possibleEdges
    in if numberOfPaths == 0
         then Left $ NotInScope (extractNodeName startNode) $ nodeInfo $ source $ head uEdges
         else if numberOfPaths == 1
                then
                    let path = head paths
                        containsTC = any (\(PathComponent _ et _) -> et == TC) path
                    in Right (path, containsTC)
                else
                    let sortedPaths = sortOn length paths
                        listOfPathsLengths = map length sortedPaths
                        shortestPathLength = head listOfPathsLengths
                        shortestPaths = takeWhile (\path -> length path == shortestPathLength) sortedPaths
                    in if length shortestPaths == 1
                          then
                                let path = head shortestPaths
                                    containsTC = any (\(PathComponent _ et _) -> et == TC) path
                                in Right (path, containsTC)
                          else Left $ MultipleDeclarations (extractNodeName startNode) (nodeInfo startNode)
