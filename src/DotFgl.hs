{-# LANGUAGE OverloadedStrings #-}

module DotFgl where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (dfs)
import DotLanguage 
import qualified Data.Map as M
import Data.Text

-- Define the nodes and edges
nodes' :: [(Node, String)]
nodes' = [(1, "A"), (2, "B"), (3, "C"), (4, "D")]

edges' :: [(Node, Node, String)]
edges' = [(1, 2, "A to B"), (2, 3, "B to C"), (3, 4, "C to D"), (4, 1, "D to A")]

fromDotGraph :: DotGraph -> Gr Text Text
fromDotGraph (Graph _ _ dot) = mkGraph nodes edges
  where
    (nodes, edges) = collectNodesAndEdges dot M.empty []

collectNodesAndEdges :: Dot -> M.Map NodeId Text 
                     -> [(Text, Text, [Attribute])] 
                     -> ([(Text, NodeId)], [(Text, Text, [Attribute])])
collectNodesAndEdges DotEmpty nodeMap edges = ([], edges)

fromDot :: DotGraph -> Gr Text Text
fromDot = undefined

-- Create a graph
graph :: Gr String String
graph = mkGraph nodes' edges'

main :: IO ()
main = do
  -- Print the graph
  putStrLn "Graph nodes:"
  print (labNodes graph)

  putStrLn "\nGraph edges:"
  print (labEdges graph)

  -- Depth-first search starting from node 1
  putStrLn "\nDFS from node 1:"
  print (dfs [1] graph)

  -- Check if there's an edge between node 1 and node 2
  putStrLn "\nIs there an edge from node 1 to node 2?"
  print (hasEdge graph (1, 2))

  -- Get the context of a node (node 2 in this example)
  putStrLn "\nContext of node 2:"
  print (context graph 2)
  print graph

