module Domain.TypeCheck.Path where
import Domain.ScopeGraph.ScopeGraph

data PathComponent = PathComponent {
    fromNode :: NodeInfo,
    edgeType :: EdgeType,
    toNode :: NodeInfo
} deriving (Eq)

instance Show PathComponent where
    show (PathComponent from et to) = show from ++ " -- " ++ show et ++ " --> " ++ show to

type Path = [PathComponent]
