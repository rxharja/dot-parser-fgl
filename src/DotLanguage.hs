module DotLanguage where

import Data.Text (Text, pack)
import Text.Trifecta
import Data.Char
import Control.Applicative ((<|>))

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

parseSpecial :: Parser Char
parseSpecial = try (char '.') <|> try (char '_') <|> alphaNum

parseValue :: Parser String
parseValue = tokenize $ try parseQuoted <|> some parseSpecial

terminator :: Parser Dot -> Parser Dot
terminator p = p <* (char '\n' <|> char ';')

parseLabel :: Parser Dot
parseLabel = tokenize $ Label . pack <$> (try (symbol "label") >> symbol "=" >> parseValue)

parseNodeId :: Parser NodeId
parseNodeId = tokenize $ (UserId . pack <$> parseValue) <|> (Nameless . fromInteger <$> decimal)

parseNode :: Parser Dot
parseNode = tokenize $ 
  Node <$> parseNodeId <*> fmap def (optional parseAttributes)

def :: Maybe [a] -> [a]
def Nothing = [] 
def (Just n) = n

-- x -> y
parseEdgeSingle :: Parser String -> Parser Dot
parseEdgeSingle connection = Edge
               <$> parseNodeId
               <* tokenize connection
               <*> parseNodeId
               <*> tokenize (fmap def (optional parseAttributes))

-- x -> y -> z
parseEdgeChain :: Parser String -> Parser Dot
parseEdgeChain connection = do
  nodes <- parseNodeId `sepBy1` tokenize connection
  attributes <- def <$> optional parseAttributes

  if length nodes < 2 
  then fail "There must be at least two nodes for a connection" 
  else return $ foldl1 (<>) $ zipWith (\a b -> Edge a b attributes) nodes (tail nodes)

-- x -> {y,z}
parseEdgeMulti :: Parser String -> Parser Dot
parseEdgeMulti connection = do
  start <- parseNodeId
  _ <- tokenize connection
  ids <- braces (parseNodeId `sepBy` tokenize (char ','))
  attributes <- def <$> optional parseAttributes

  if null ids
  then fail "There must be at least one other node in { }" 
  else return $ foldl1 (<>) $ (\x -> Edge start x attributes) <$> ids

parseEdge :: Parser String -> Parser Dot
parseEdge connection = choice 
  [ try (parseEdgeMulti connection)
  , try (parseEdgeChain connection)
  , parseEdgeSingle connection ] 

parseRankSame :: Parser Dot
parseRankSame = undefined -- todo

parseDecType :: Parser DecType
parseDecType =tokenize 
              (DecNode <$ insensitiveString "node" 
           <|> DecEdge <$ insensitiveString "edge"
           <|> DecGraph <$ insensitiveString "graph")

parseDec :: Parser Dot
parseDec = Declaration <$> parseDecType <*> parseAttributes

parseAttribute :: Parser Attribute
parseAttribute = 
  let couple x y = (pack x, pack y)
   in couple <$> tokenize (many alphaNum) <* symbol "=" <*> tokenize parseValue  

parseAttributes :: Parser [Attribute]
parseAttributes = tokenize $ 
  brackets (parseAttribute `sepBy` tokenize (char ',' <|> char ';')) <* optional (char ';')

parseSubGraph :: Parser String -> Parser Dot
parseSubGraph con = Subgraph 
                 <$ tokenize (insensitiveString "subgraph")
                 <*> fmap pack parseValue 
                 <*> braces (parseDot con)

parseDot :: Parser String -> Parser Dot
parseDot con =
  let captureDot p = whiteSpaceOrComment >> p <* optional (tokenize $ char ';')
      dotParsers = choice $ captureDot <$> 
                  [ try parseLabel
                  , try parseRankdir
                  , try $ parseEdge con  
                  , try $ parseSubGraph con  
                  , try parseDec
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

