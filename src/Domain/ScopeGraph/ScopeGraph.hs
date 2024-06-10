module Domain.ScopeGraph.ScopeGraph where
import Domain.Language.LanguageComponents

data NodeInfo =
    ScopeNode String
    | DeclNode String Type
    | TypeClassNode TypeClass TypeVar
    | InstanceNode TypeClass Type
    | UsageNode String deriving (Eq)

instance Show NodeInfo where
    show (ScopeNode desc) = desc
    show (DeclNode name t) = name ++ ": " ++ show t
    show (TypeClassNode (TypeClass tc) (TypeVar tv)) = tc ++ ": TCLASS(" ++ tv ++ ")"
    show (InstanceNode (TypeClass tc) t) = show t ++ ": INST(" ++ tc ++ ")"
    show (UsageNode name) = name

data Node = Node { label :: Int, nodeInfo :: NodeInfo } deriving (Eq)
instance Show Node where
    show (Node nodeLabel info) = show nodeLabel ++ ": " ++ show info

data EdgeType = D | TC | I | P | Eq | U deriving (Show, Eq)

data Edge = Edge { source :: Node, destination :: Node, edgeType :: EdgeType }
instance Show Edge where
    show (Edge from to et) = show (label from) ++ " -- " ++ show et ++ " --> " ++ show (label to)

data ScopeGraph = ScopeGraph { nodes :: [Node], edges :: [Edge], nextLabel :: Int }

emptyScopeGraph :: ScopeGraph
emptyScopeGraph = ScopeGraph [] [] 0

addNode :: NodeInfo -> ScopeGraph -> (Node, ScopeGraph)
addNode info sg = 
    let nodeId = nextLabel sg
        newNode = Node nodeId info
        newGraph = sg { nodes = newNode : nodes sg, nextLabel = nodeId + 1 }
    in (newNode, newGraph)

addEdge :: Node -> Node -> EdgeType -> ScopeGraph -> ScopeGraph
addEdge from to et sg = 
    let newEdge = Edge from to et
    in sg { edges = newEdge : edges sg }

extractUsageNodeInfos :: ScopeGraph -> [Node]
extractUsageNodeInfos sg = filter (\n -> case nodeInfo n of 
                                         UsageNode _ -> True 
                                         _ -> False) (nodes sg)
