module DotFgl where

import Data.Graph.Inductive.PatriciaTree ( Gr, Gr )
import DotLanguage
import qualified Data.Map as M
import qualified Data.Graph.Inductive.Graph as G

type FGLGraph = Gr NodeId [Attribute]

dotGraphToFGL :: DotGraph -> FGLGraph
dotGraphToFGL (Graph _ _ dot) = G.mkGraph nodes edges
  where
    (nodes, edges) = collectNodesAndEdges dot M.empty []

collectNodesAndEdges :: Dot 
                     -> M.Map NodeId Int 
                     -> [(Int, Int, [Attribute])] 
                     -> ([(Int, NodeId)], [(Int, Int, [Attribute])])
collectNodesAndEdges (Node nodeId _) nodeMap edges =
  let (_, nodeIdx) = addNode nodeId nodeMap
  in ([(nodeIdx, nodeId)], edges)

collectNodesAndEdges (Edge n1 n2 attrs) nodeMap edges =
  let (nodeMap', nodeIdx1) = addNode n1 nodeMap
      (_, nodeIdx2) = addNode n2 nodeMap'
      newEdge = (nodeIdx1, nodeIdx2, attrs)
  in ([], newEdge : edges)

collectNodesAndEdges (DotSeq d1 d2) nodeMap edges =
  let (nodes1, edges1) = collectNodesAndEdges d1 nodeMap edges
      (nodes2, edges2) = collectNodesAndEdges d2 nodeMap edges1
  in (nodes1 ++ nodes2, edges2)

collectNodesAndEdges (Subgraph _ dot) nodeMap edges = 
  collectNodesAndEdges dot nodeMap edges

collectNodesAndEdges _ _ edges = ([], edges)

addNode :: NodeId -> M.Map NodeId Int -> (M.Map NodeId Int, Int)
addNode nodeId nodeMap = case M.lookup nodeId nodeMap of
  Just idx -> (nodeMap, idx)
  Nothing  -> let idx = M.size nodeMap in (M.insert nodeId idx nodeMap, idx)

