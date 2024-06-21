module Domain.TypeCheck.Path where
import Domain.ScopeGraph.ScopeGraph

data PathComponent = PathComponent {
    fromNode :: Node,
    edgeT :: EdgeType,
    toNode :: Node
} deriving (Eq)

instance Show PathComponent where
    show (PathComponent from et to) = show (nodeInfo from) ++ " --" ++ show et ++ "--> " ++ show (nodeInfo to)

type Path = [PathComponent]
