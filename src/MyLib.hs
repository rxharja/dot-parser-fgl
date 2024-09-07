module MyLib where


{-# LANGUAGE OverloadedStrings #-}

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (dfs)

-- infixr :&:

-- type Node = Int

-- type Adj b = [(b, Node)]

-- type Context a b = (Adj b, Node, a, Adj b)

-- data Graph a b = Empty | Context a b :&: Graph a b

-- newGraph :: Graph Char String
-- newGraph = 
--   ([("left", 2), ("up", 3)], 1, 'a', [("right", 2)]) :&:
--                         ([], 2, 'b', [("down", 3)])  :&:
--                         ([], 3, 'c', [])             :&: Empty

-- newGraph' :: Graph Char String
-- newGraph' = 
--   ([("down", 2)],  3, 'c', [("up", 1)])   :&:
--   ([("right", 1)], 2, 'b', [("left", 1)]) :&:
--               ([], 1, 'a', [])            :&: Empty

-- isEmpty :: Graph a b -> Bool
-- isEmpty Empty = True
-- isEmpty _     = False

-- gmap :: (Context a b -> Context c d) -> Graph a b -> Graph c d
-- gmap _ Empty     = Empty
-- gmap f (c :&: g) = f c :&: gmap f g

-- Define the nodes and edges
nodes' :: [(Node, String)]
nodes' = [(1, "A"), (2, "B"), (3, "C"), (4, "D")]

edges' :: [(Node, Node, String)]
edges' = [(1, 2, "A to B"), (2, 3, "B to C"), (3, 4, "C to D"), (4, 1, "D to A")]

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

