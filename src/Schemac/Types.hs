module Schemac.Types where

import GHC.Generics
import Data.Hashable

data Prim = Prim
    { primId :: Int
    , primName :: String
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Prim

data Tag = Tag
    { tagId :: Int
    , tagName :: String
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Tag

data Case = Case
    { caseId :: Int
    , caseLinks :: [Link]
    , caseName :: String
    , caseProps :: [Prop]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Case

data Data = Data
    { dataId :: Int
    , dataName :: String
    , dataCases :: [Case]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Data

data Type
    = PrimType Prim
    | DataType Data
    deriving (Show, Eq, Ord, Generic)
instance Hashable Type

data Trait = Trait
    { traitId :: Int
    , traitLinks :: [Link]
    , traitName :: String
    , traitParents :: [Trait]
    , traitProps :: [Prop]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Trait

data Entity = Entity
    { entityId :: Int
    , entityName :: String
    , entityLinks :: [Link]
    , entityProps :: [Prop]
    , entityTraits :: [Trait]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Entity

data Link = Link
    { linkId :: Int
    , linkEntity :: Entity
    , linkName :: String
    , linkTags :: [Tag]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Link

data Prop = Prop
    { propId :: Int
    , propName :: String
    , propTags :: [Tag]
    , propType :: Type
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Prop

data Schema = Schema
    { schemaId :: Int
    , schemaDatas :: [Data]
    , schemaEntities :: [Entity]
    , schemaName :: String
    , schemaPrims :: [Prim]
    , schemaTags :: [Tag]
    , schemaTraits :: [Trait]
    } deriving (Show, Eq)
    
data Emit
    = CaseLink Int Int
    | CaseName Int String
    | CaseProp Int Int
    | DataCase Int Int
    | DataName Int String
    | DecCase Int
    | DecData Int
    | DecEntity Int
    | DecLink Int
    | DecPrim Int
    | DecProp Int
    | DecSchema Int
    | DecTag Int
    | DecTrait Int
    | EntityLink Int Int
    | EntityName Int String
    | EntityProp Int Int
    | EntityTrait Int Int
    | LinkEntity Int Int
    | LinkTag Int Int
    | LinkName Int String
    | PrimName Int String
    | PropData Int Int
    | PropName Int String
    | PropPrim Int Int
    | PropTag Int Int
    | SchemaData Int Int
    | SchemaEntity Int Int
    | SchemaName Int String
    | SchemaPrim Int Int
    | SchemaTag Int Int
    | SchemaTrait Int Int
    | TagName Int String
    | TraitLink Int Int
    | TraitName Int String
    | TraitParent Int Int
    | TraitProp Int Int
    deriving (Eq, Show)
