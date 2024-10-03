module DotFgl where

import Data.Graph.Inductive.PatriciaTree ( Gr, Gr )
import DotLanguage
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Hashable as Hash

type Nodes = [(Int, NodeId)]
type Edges = [(Int, Int, [Attribute])]
type FGLGraph = Gr NodeId [Attribute]

dotGraphToFGL :: DotGraph -> FGLGraph
dotGraphToFGL (Graph _ _ dot) = G.mkGraph nodes edges
  where
    (nodes, edges) = collectNodesAndEdges dot []

collectNodesAndEdges :: Dot -> Edges -> (Nodes, Edges)
collectNodesAndEdges (Node nodeId _) edges =
  ([(hashNode nodeId, nodeId)], edges)

collectNodesAndEdges (Edge n1 n2 attrs) edges =
  let nodeIdx1 = hashNode n1 
      nodeIdx2 = hashNode n2 
      newEdge = (nodeIdx1, nodeIdx2, attrs)
  in ([(nodeIdx1, n1), (nodeIdx2, n2)], newEdge : edges)

collectNodesAndEdges (DotSeq d1 d2) edges =
  let (nodes1, edges') = collectNodesAndEdges d1 edges
      (nodes2, edges'') = collectNodesAndEdges d2 edges'
  in (nodes1 ++ nodes2, edges'')

collectNodesAndEdges (Subgraph _ dot) edges = 
  collectNodesAndEdges dot edges

collectNodesAndEdges (Ranksame dot) edges = 
  collectNodesAndEdges dot edges

collectNodesAndEdges _ edges = ([], edges)

hashNode :: NodeId -> Int
hashNode (Nameless x) = x
hashNode (UserId name) = Hash.hash name

