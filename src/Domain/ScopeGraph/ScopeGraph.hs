module Domain.ScopeGraph.ScopeGraph where
import Domain.Language.LanguageComponents
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

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
    show (Edge from to et) = show (nodeInfo from) ++ " -- " ++ show et ++ " --> " ++ show (nodeInfo to)

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

declNodeNameMatches :: Node -> String -> Bool
declNodeNameMatches (Node _ (DeclNode "Number" _)) targetName = length targetName == length (filter id $ map isDigit targetName)
declNodeNameMatches (Node _ (DeclNode name _)) targetName = name == targetName
declNodeNameMatches _ _ = False

extractNodeName :: Node -> String
extractNodeName (Node _ (UsageNode name)) = name
extractNodeName (Node _ (DeclNode name _)) = name
extractNodeName (Node _ (TypeClassNode (TypeClass tc) _)) = tc
extractNodeName _ = error "Node does not have a name"

type SearchState = Map String Int

getUsageNodeFromName :: String -> ScopeGraph -> SearchState -> (Maybe Node, SearchState)
getUsageNodeFromName name sg searchCounts =
    let usageNodes = filter (\(Node _ info) -> case info of
                                                UsageNode n -> n == name
                                                _ -> False) (nodes sg)
        sortedNodesByLabel = sortOn label usageNodes
        currentCount = Map.findWithDefault 0 name searchCounts
        newCount = currentCount + 1
        updatedCounts = Map.insert name newCount searchCounts
    in if length sortedNodesByLabel >= newCount
       then (Just (sortedNodesByLabel !! (newCount - 1)), updatedCounts)
       else (Nothing, updatedCounts)

getTypeFromNode :: Node -> Type
getTypeFromNode (Node _ (DeclNode _ t)) = t
getTypeFromNode _ = error "Node does not have a type"
