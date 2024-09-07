{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module DotLanguage where

import Data.Text (Text, pack)
import Text.Trifecta
import Data.Char
import Control.Applicative ((<|>))

default (Text)

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

instance Monoid Dot where
  mempty = DotEmpty

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

parseRankdir :: Parser RankdirType
parseRankdir = tokenize $ symbol "rankdir" >> symbol "=" >> optionalQuotes parseRankDirVal

parseValue :: Parser String
parseValue = tokenize $ try parseQuoted <|> some alphaNum

parseLabel :: Parser Dot
parseLabel = tokenize $ Label . pack <$> (symbol "label" >> symbol "=" >> parseValue)

parseNodeId :: Parser NodeId
parseNodeId = tokenize $ (UserId . pack <$> parseValue) <|> (Nameless . fromInteger <$> decimal)            

parseNode :: Parser Dot
parseNode = tokenize $ Node <$> parseNodeId <*> pure [] -- todo: parse attributes

parseEdge :: Parser String -> Parser Dot
parseEdge connection = do
  start <- parseNodeId
  _ <- tokenize connection
  stop <- parseNodeId
  return $ Edge start stop []

conStyle :: GraphType -> Parser String
conStyle g = if g == DirectedGraph then symbol "->" else symbol "--"

parseDot :: Parser String -> Parser Dot
parseDot con = foldl1 (<>) <$> many (parseEdge con <|> parseLabel)

parseGraph :: Parser DotGraph 
parseGraph = do
  graphType <- parseGraphType  
  graphName <- pack <$> parseValue
  body <- between (char '{') (char '}') (parseDot $ conStyle graphType)
  return $ Graph graphType graphName body
