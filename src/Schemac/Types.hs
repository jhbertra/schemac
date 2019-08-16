module Schemac.Types where

newtype Prim = Prim { unPrim :: String } deriving (Show, Eq, Ord)
newtype Tag = Tag { unTag :: String } deriving (Show, Eq, Ord)

data Case = Case
    { caseLinks :: [Link]
    , caseName :: String
    , caseProps :: [Prop]
    } deriving (Show, Eq, Ord)

newtype Data = Data { unData :: [Case] } deriving (Show, Eq, Ord)

data Type
    = PrimType Prim
    | DataType Data
    deriving (Show, Eq, Ord)

data Modifiers
    = None
    | Option
    | Many
    | ManyOption
    deriving (Show, Eq, Ord)

data Trait = Trait
    { traitLinks :: [Link]
    , traitName :: String
    , traitParents :: [Trait]
    , traitProps :: [Prop]
    } deriving (Show, Eq, Ord)

data Entity = Entity
    { entityName :: String
    , entityLinks :: [Link]
    , entityProps :: [Prop]
    , entityTraits :: [Trait]
    } deriving (Show, Eq, Ord)

data Link = Link
    { linkEntity :: Entity
    , linkName :: String
    , linkModifiers :: Modifiers
    , linkTags :: [Tag]
    } deriving (Show, Eq, Ord)

data Prop = Prop
    { propName :: Entity
    , propModifiers :: Modifiers
    , propTags :: [Tag]
    , propType :: Type
    } deriving (Show, Eq, Ord)

data Schema = Schema
    { schemaPrims :: [Prim]
    , schemaRoot :: Entity
    , schemaTags :: [Tag]
    , schemaTraits :: [Trait]
    } deriving (Show, Eq)