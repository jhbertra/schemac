{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Schemac.Parse
    ( AST(..)
    , SchemaNode(..)
    , sNodeName
    , sNodeMembers
    , SchemaMemberNode(..)
    , _DataMember
    , _EntityMember
    , _PrimMember
    , _TagMember
    , DataNode(..)
    , dNodeSourcePos
    , dNodeName
    , dNodeCases
    , EntityNode(..)
    , eNodeSourcePos
    , eNodeName
    , eNodeFields
    , PrimNode(..)
    , pNodeSourcePos
    , pNodeName
    , TagNode(..)
    , tNodeSourcePos
    , tNodeName
    , CaseNode(..)
    , cNodeSourcePos
    , cNodeName
    , cNodeFields
    , Field(..)
    , _PropField
    , _LinkField
    , PropNode(..)
    , prNodeSourcePos
    , prNodeName
    , prNodeType
    , prNodeTags
    , LinkNode(..)
    , lNodeSourcePos
    , lNodeName
    , lNodeType
    , lNodeTags
    , SchemaName
    , CaseName
    , DataName
    , EntityName
    , LinkName
    , PrimName
    , PropName
    , TagName
    , TypeName
    , TypeNode(..)
    , tyNodeSourcePos
    , tyNodeName
    , parseSchema
    ) where

import Control.Monad
import Control.Lens

import Data.ByteString (ByteString)

import Text.Parsec hiding (State)
import Text.Parsec.Indent

newtype AST = AST { unAST :: SchemaNode } deriving (Show)

data SchemaNode = SchemaNode
    { _sNodeName :: SchemaName
    , _sNodeMembers :: [SchemaMemberNode]
    } deriving (Show)

data SchemaMemberNode
    = DataMember DataNode
    | EntityMember EntityNode
    | PrimMember PrimNode
    | TagMember TagNode
    deriving (Show)

data DataNode = DataNode
    { _dNodeSourcePos :: SourcePos
    , _dNodeName :: DataName
    , _dNodeCases :: [CaseNode]
    } deriving (Show)

data EntityNode = EntityNode
    { _eNodeSourcePos :: SourcePos
    , _eNodeName :: EntityName
    , _eNodeFields :: [Field]
    } deriving (Show)

data PrimNode = PrimNode
    { _pNodeSourcePos :: SourcePos
    , _pNodeName :: PrimName
    } deriving (Show)

data TagNode = TagNode
    { _tNodeSourcePos :: SourcePos
    , _tNodeName :: TagName
    } deriving (Show)

data CaseNode = CaseNode
    { _cNodeSourcePos :: SourcePos
    , _cNodeName :: CaseName
    , _cNodeFields :: [Field]
    } deriving (Show)

data Field
    = PropField PropNode
    | LinkField LinkNode
    deriving (Show)

data PropNode = PropNode
    { _prNodeSourcePos :: SourcePos
    , _prNodeName :: PropName
    , _prNodeType :: TypeNode
    , _prNodeTags :: [(TagName, SourcePos)]
    } deriving (Show)

data LinkNode = LinkNode
    { _lNodeSourcePos :: SourcePos
    , _lNodeName :: LinkName
    , _lNodeType :: TypeNode
    , _lNodeTags :: [(TagName, SourcePos)]
    } deriving (Show)

data TypeNode = TypeNode
    { _tyNodeSourcePos :: SourcePos
    , _tyNodeName :: TypeName
    } deriving (Show)

type SchemaName = String
type CaseName = String
type DataName = String
type EntityName = String
type LinkName = String
type PrimName = String
type PropName = String
type TagName = String
type TypeName = String
    
makeLenses ''SchemaNode
makePrisms ''SchemaMemberNode
makeLenses ''DataNode
makeLenses ''CaseNode
makeLenses ''EntityNode
makeLenses ''PrimNode
makeLenses ''TagNode
makePrisms ''Field
makeLenses ''PropNode
makeLenses ''LinkNode
makeLenses ''TypeNode

type Parser a = IndentParser ByteString () a

parseSchema :: SourceName -> ByteString -> Either ParseError AST
parseSchema = runIndentParser schema ()

schema :: Parser AST
schema = fmap AST $ SchemaNode
    <$> fmap snd (declaration "schema")
    <*> option [] (block member)
    <* eof

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum <?> "Identifier"

member :: Parser SchemaMemberNode
member = (DataMember <$> data' <?> "data definition")
    <|> (EntityMember <$> entity <?> "entity definition")
    <|> (PrimMember <$> prim <?> "prim definition")
    <|> (TagMember <$> tag <?> "tag definition")

data' :: Parser DataNode
data' = withBlock
    (uncurry DataNode)
    (declaration "data")
    (case' <?> "case definition")

entity :: Parser EntityNode
entity = withBlock
    (uncurry EntityNode)
    (declaration "entity")
    field

prim :: Parser PrimNode
prim = uncurry PrimNode <$> declaration "prim" <?> "prim definition"

tag :: Parser TagNode
tag = uncurry TagNode <$> declaration "tag" <?> "tag definition"

case' :: Parser CaseNode
case' = withBlock
    (uncurry CaseNode)
    (declaration "case")
    field

field :: Parser Field
field = (PropField <$> prop <?> "prop definition")
    <|> (LinkField <$> link <?> "link definition")

prop :: Parser PropNode
prop = uncurry PropNode
    <$> declaration "prop"
    <* spaces
    <*> type'
    <* spaces
    <*> tags

type' :: Parser TypeNode
type' = TypeNode
    <$> sourcePos
    <*> identifier

link :: Parser LinkNode
link = uncurry LinkNode
    <$> declaration "link"
    <* spaces
    <*> type'
    <* spaces
    <*> tags

tags :: Parser [(TagName, SourcePos)]
tags = many $ char '#' *> (flip (,) <$> sourcePos <*> identifier) <* spaces

declaration :: String -> Parser (SourcePos, String)
declaration keyword = string keyword *> spaces *> ((,) <$> sourcePos <*> identifier) <* spaces <?> keyword ++ " declaration"

sourcePos :: Parser SourcePos
sourcePos = statePos `liftM` getParserState
