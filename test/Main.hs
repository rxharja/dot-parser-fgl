{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Text.RawString.QQ (r)

input :: String
input = [r| 
digraph "test" {
  /* 
    j -> k -> l
    a multi-line comment
    a multi-line comment
    a multi-line comment
    a multi-line comment
    a multi-line comment
  */
  label = "test label"
  rankdir=BT // flip this bad boy around
  a -> {b,c,d}
  x->y->z
} |]

input' :: String
input' = [r| 

GRAPH "not directed" {
  // a test
  label = "Pretty Picture"
  rankdir=LR 
  b // I like this guy
  a--{b,c,d}
  x--y--z
}

|]

inputOnline :: String
inputOnline = [r|

digraph G {

  subgraph cluster_1 {
    node [style=filled,color=white];
    b0 -> b1 -> b2 -> b3;
    label = "process #2";
  }

  start -> a0;
  start -> b0;
  a1 -> b3;
  b2 -> a3 [color=red, width=1.4];
  a3 -> a0;
  a3 -> end;
  b3 -> end;

  start [shape=Mdiamond];
  end [shape=Msquare];
}

|]

main :: IO ()
main = putStrLn "Test suite not yet implemented."
