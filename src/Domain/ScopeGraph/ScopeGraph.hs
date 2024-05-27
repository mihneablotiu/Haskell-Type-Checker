module Domain.ScopeGraph.ScopeGraph where

import Domain.Language.LanguageComponents
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (unpack)

data NodeInfo =
    ScopeNode
    | DeclNode String Type
    | TypeClassNode TypeClass
    | InstanceNode TypeClass Type
    deriving (Show)

data Node = Node { label :: Int, nodeInfo :: NodeInfo } deriving (Show)
data EdgeType = D | TC | I | P deriving (Show)

data Edge = Edge 
    { source :: Node,
      destination :: Node,
      edgeType :: EdgeType
    } deriving (Show)

data ScopeGraph = ScopeGraph 
    { nodes :: [Node],
      edges :: [Edge],
      nextLabel :: Int
    } deriving (Show)


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

scopeGraphToDot :: ScopeGraph -> String
scopeGraphToDot sg = unpack $ renderDot $ toDot $ do
    graph' $ do
        mapM_ (\(Node nid info) -> node (show nid) [toLabel (show info)]) (nodes sg)
        mapM_ (\(Edge (Node from _) (Node to _) et) -> edge (show from) (show to) [toLabel (show et)]) (edges sg)
