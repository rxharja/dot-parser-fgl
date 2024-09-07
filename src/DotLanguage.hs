{-# LANGUAGE QuasiQuotes #-}

module DotLanguage where

import Data.Text (Text, pack)
import Text.Trifecta
import Data.Char
import Control.Applicative ((<|>))
import Text.RawString.QQ (r)

type GraphName = Text

data GraphType = UndirectedGraph
               | DirectedGraph
  deriving (Show, Eq)

type AttributeName = Text

type AttributeValue = Text

type Attribute = (AttributeName, AttributeValue)

data NodeId = UserId Text
            | Nameless Int
  deriving (Show, Eq)

data DecType = DecGraph
             | DecNode
             | DecEdge
  deriving (Show, Eq)

data DotGraph = Graph GraphType GraphName Dot deriving (Show, Eq)

data RankdirType = LR
                 | RL
                 | TB
                 | BT
  deriving (Show, Eq)

data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId [Attribute]
         | Declaration DecType [Attribute]
         | Ranksame Dot
         | Subgraph Text Dot
         | RawDot Text
         | Label Text
         | Rankdir RankdirType
         | DotSeq Dot Dot
         | DotEmpty
  deriving (Show, Eq)

instance Semigroup Dot where
  DotEmpty <> d = d
  d <> DotEmpty = d
  d <> (DotSeq d1 d2) = DotSeq (d <> d1) d2
  d1 <> d2 = DotSeq d1 d2

instance Monoid Dot where mempty = DotEmpty

skipMany1 :: Parsing f => f a -> f ()
skipMany1 p = p *> skipMany p

insensitiveString :: String -> Parser String
insensitiveString = traverse ci where ci c = char (toLower c) <|> char (toUpper c)

betweenBrackets :: Parser a -> Parser a
betweenBrackets = between (char '[') (char ']')

optionalQuotes :: Parser a -> Parser a
optionalQuotes p = between (char '\"') (char '\"') p <|> p

parseQuoted :: Parser String
parseQuoted = char '\"' >> manyTill anyChar (char '\"')

-- comments
singlelineComment :: Parser ()
singlelineComment = string "//" >> anyChar `manyTill` newline >> return ()

multilineComment :: Parser ()
multilineComment = string "/*" >> anyChar `manyTill` string "*/" >> return ()

skipComment :: Parser ()
skipComment = try multilineComment <|> singlelineComment

whiteSpaceOrComment :: Parser ()
whiteSpaceOrComment = skipMany (skipComment <|> skipMany1 space)

parseEmptySpace :: Parser ()
parseEmptySpace = choice [skipComment, skipMany newline, skipMany space]

parseInlineSpace :: Parser ()
parseInlineSpace = choice [multilineComment, skipMany newline, skipMany space]

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpaceOrComment

parseKvp :: String -> Parser b -> Parser b
parseKvp k parseV = string k
                 >> whiteSpaceOrComment
                 >> char '='
                 >> whiteSpaceOrComment
                 >> parseV

-- graph type
parseGraphType :: Parser GraphType
parseGraphType = tokenize $ (DirectedGraph <$ insensitiveString "digraph")
             <|> (UndirectedGraph <$ insensitiveString "graph")

-- rank dir
parseRankDirVal :: Parser RankdirType
parseRankDirVal =
  choice [ LR <$ string "LR"
         , RL <$ string "RL"
         , TB <$ string "TB"
         , BT <$ string "BT" ]

parseRankdir :: Parser Dot
parseRankdir = Rankdir <$> tokenize (symbol "rankdir" >> symbol "=" >> optionalQuotes parseRankDirVal)

parseValue :: Parser String
parseValue = tokenize $ try parseQuoted <|> some alphaNum

terminator :: Parser Dot -> Parser Dot
terminator p = p <* (char '\n' <|> char ';')

parseLabel :: Parser Dot
parseLabel = tokenize $ Label . pack <$> (try (symbol "label") >> symbol "=" >> parseValue)

parseNodeId :: Parser NodeId
parseNodeId = tokenize $ (UserId . pack <$> parseValue) <|> (Nameless . fromInteger <$> decimal)

parseNode :: Parser Dot
parseNode = tokenize $ Node <$> parseNodeId <*> pure []

-- x -> y
parseEdgeSingle :: Parser String -> Parser Dot
parseEdgeSingle connection = Edge
               <$> parseNodeId
               <* tokenize connection
               <*> parseNodeId
               <*> pure []

-- x -> y -> z
parseEdgeChain :: Parser String -> Parser Dot
parseEdgeChain connection = do
  nodes <- parseNodeId `sepBy1` tokenize connection

  if length nodes < 2 
  then fail "There must be at least two nodes for a connection" 
  else return $ foldl1 (<>) $ zipWith (\a b -> Edge a b []) nodes (tail nodes)

-- x -> {y,z}
parseEdgeMulti :: Parser String -> Parser Dot
parseEdgeMulti connection = do
  start <- parseNodeId
  _ <- tokenize connection
  ids <- braces (parseNodeId `sepBy` tokenize (char ','))

  if null ids
  then fail "There must be at least one other node in { }" 
  else return $ foldl1 (<>) $ (\x -> Edge start x []) <$> ids

parseEdge :: Parser String -> Parser Dot
parseEdge connection = choice 
  [ try (parseEdgeMulti connection)
  , try (parseEdgeChain connection)
  , parseEdgeSingle connection ] 

parseAttributes :: Parser [Attribute]
parseAttributes = undefined -- todo

parseSubGraph :: Parser Dot
parseSubGraph = undefined -- todo

parseDot :: Parser String -> Parser Dot
parseDot con =
  let captureDot p = whiteSpaceOrComment >> p <* optional (tokenize $ char ';')
      dotParsers = choice $ captureDot <$> 
                  [ try parseLabel
                  , try parseRankdir
                  , try $ parseEdge con  
                  , parseNode ]
   in foldl1 (<>) <$> many dotParsers

conStyle :: GraphType -> Parser String
conStyle UndirectedGraph = string "--"
conStyle   DirectedGraph = string "->"

parseGraph :: Parser DotGraph
parseGraph = do
  whiteSpaceOrComment
  graphType <- parseGraphType
  graphName <- pack <$> parseValue
  body <- braces $ parseDot (conStyle graphType)
  return $ Graph graphType graphName body

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
  start -> a0;
  start -> b0;
  a1 -> b3;
  b2 -> a3;
  a3 -> a0;
  a3 -> end;
  b3 -> end;

  start;
  end;
}

|]
