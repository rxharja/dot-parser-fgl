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
whiteSpaceOrComment = try multilineComment <|> whiteSpace

parseEmptySpace :: Parser ()
parseEmptySpace = choice [skipComment, skipMany newline, skipMany space]

parseInlineSpace :: Parser ()
parseInlineSpace = choice [multilineComment, skipMany newline, skipMany space]

parseKvp :: String -> Parser b -> Parser b
parseKvp k parseV = string k 
                 >> whiteSpaceOrComment 
                 >> char '=' 
                 >> whiteSpaceOrComment 
                 >> parseV

-- graph type
parseGraphType :: Parser GraphType
parseGraphType = (DirectedGraph <$ insensitiveString "digraph") 
             <|> (UndirectedGraph <$ insensitiveString "graph")

-- rank dir
parseRankDirVal :: Parser RankdirType
parseRankDirVal = 
  choice [ LR <$ string "LR" 
         , RL <$ string "RL" 
         , TB <$ string "TB" 
         , BT <$ string "BT" ]

parseRankDir :: Parser RankdirType
parseRankDir = parseKvp "rankdir" (optionalQuotes parseRankDirVal)

parseValue :: Parser String
parseValue = parseQuoted <|> some alphaNum

parseLabel :: Parser Dot
parseLabel = Label . pack <$> parseKvp "label" parseValue
