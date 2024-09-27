{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Text.RawString.QQ (r)
import Text.Trifecta
import Test.Hspec
import DotLanguage

complexInput :: String
complexInput = [r|

/* 
  this graph describes two subgraphs
  titled cluster_0 and cluster_1.

  Cluster 0 groups nodes a0 -> a3
  and Cluster 1 groups nodes b0 -> b3.
*/

digraph G {

  /* Cluster 0 groups nodes a0 -> a3 */

  subgraph cluster_0 {
    style=filled;
    color=lightgrey; 
    /* light 
    grey */
    node [style=filled,color=white];
    a0 -> a1 -> a2 -> a3;
    label = "process #1";
  }

  /* and Cluster 1 groups nodes b0 -> b3. */

  subgraph cluster_1 {
    node [style=filled];
    b0 -> b1 -> b2 -> b3;
    label = "process #2";
    color=blue // optional style parameter
  }
  start -> a0;
  start -> b0;
  a1 -> b3;
  b2 -> a3;
  a3 -> a0;
  a3 -> end;
  b3 -> end;

  start [shape=Mdiamond];
  end [shape=Msquare];
  {RANK = SAME; q1; q2}
}

|]

result :: DotGraph
result = 
  Graph DirectedGraph "G" (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq 
    (Subgraph "cluster_0" (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq 
      (Attribute ("style","filled")) 
      (Attribute ("color", "lightgrey"))) 
      (Declaration DecNode [("style","filled"),("color","white")])) 
      (Edge (UserId "a0") (UserId "a1") [])) 
      (Edge (UserId "a1") (UserId "a2") [])) 
      (Edge (UserId "a2") (UserId "a3") [])) 
      (Label "process #1"))) 

    (Subgraph "cluster_1" (DotSeq (DotSeq (DotSeq (DotSeq (DotSeq 
      (Declaration DecNode [("style","filled")]) 
      (Edge (UserId "b0") (UserId "b1") [])) 
      (Edge (UserId "b1") (UserId "b2") [])) 
      (Edge (UserId "b2") (UserId "b3") [])) 
      (Label "process #2")) 
      (Attribute ("color","blue"))))) 

    (Edge (UserId "start") (UserId "a0") [])) 
    (Edge (UserId "start") (UserId "b0") [])) 
    (Edge (UserId "a1") (UserId "b3") [])) 
    (Edge (UserId "b2") (UserId "a3") [])) 
    (Edge (UserId "a3") (UserId "a0") [])) 
    (Edge (UserId "a3") (UserId "end") [])) 
    (Edge (UserId "b3") (UserId "end") [])) 

    (Node (UserId "start") [("shape","Mdiamond")])) 
    (Node (UserId "end") [("shape","Msquare")])) 
    (Ranksame (DotSeq (Node (UserId "q1") []) (Node (UserId "q2") []))))

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "Successfully parses the complex input" $ do
       case parseString parseGraph mempty complexInput of
         Success _ -> return ()
         Failure e -> do
          print e >> putStrLn "\n"
          fail "parsing failed"

    it "Produces a result that matches the expected outcome" $ do
       case parseString parseGraph mempty complexInput of
         Success parsed -> parsed `shouldBe` result
         Failure _ -> fail "parsing failed"
