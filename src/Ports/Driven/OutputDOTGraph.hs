module Ports.Driven.OutputDOTGraph where

import Domain.ScopeGraph.ScopeGraph
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (unpack)

-- Convert a ScopeGraph to DOT format
scopeGraphToDot :: ScopeGraph -> String
scopeGraphToDot sg = unpack $ renderDot $ toDot $ do
    graph' $ do
        mapM_ (\(Node nid info) -> node (show nid) [toLabel (show info), shapeAttr info]) (nodes sg)
        mapM_ (\(Edge (Node from _) (Node to _) et) -> edge (show from) (show to) [toLabel (show et)]) (edges sg)
  where
    shapeAttr (ScopeNode _) = shape Ellipse
    shapeAttr (DeclNode _ _) = shape BoxShape
    shapeAttr (TypeClassNode _ _) = shape BoxShape
    shapeAttr (InstanceNode _ _) = shape BoxShape
