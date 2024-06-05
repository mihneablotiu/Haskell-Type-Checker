module Ports.Driven.OutputDOTGraph where

import Domain.ScopeGraph.ScopeGraph
import Data.GraphViz
import Data.GraphViz.Printing (renderDot)
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy (unpack)
import Data.GraphViz.Attributes.Complete

-- Convert a ScopeGraph to DOT format
scopeGraphToDot :: ScopeGraph -> String
scopeGraphToDot sg = unpack $ renderDot $ toDot $ do
    graph' $ do
        mapM_ (\(Node nid info) -> node (show nid) (attributes info)) (nodes sg)
        mapM_ (\(Edge (Node from _) (Node to _) et) -> edgeDirection (show from) (show to) et) (edges sg)
  where
    attributes info = [toLabel (show info), shapeAttr info] ++ styleAttr info
    shapeAttr (ScopeNode _) = shape Ellipse
    shapeAttr (DeclNode _ _) = shape BoxShape
    shapeAttr (TypeClassNode _ _) = shape BoxShape
    shapeAttr (InstanceNode _ _) = shape BoxShape
    shapeAttr (UsageNode _) = shape BoxShape
    
    styleAttr (UsageNode _) = [Style [SItem Dashed []]]
    styleAttr _ = []

    edgeDirection from to U = edge from to [toLabel "U", Dir Back]
    edgeDirection from to P = edge from to [toLabel "P", Dir Back]
    edgeDirection from to Eq = edge from to [toLabel "Eq", Dir Both, ArrowHead (AType [(ArrMod FilledArrow BothSides, DotArrow)]),
                                             ArrowTail (AType [(ArrMod FilledArrow BothSides, DotArrow)])]
    edgeDirection from to et = edge from to [toLabel (show et), Dir Forward, ArrowHead (AType [(ArrMod FilledArrow BothSides, DotArrow)])]
