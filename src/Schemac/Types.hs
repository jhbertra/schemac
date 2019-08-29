module Schemac.Types where

import GHC.Generics
import Data.Hashable
import Text.Parsec

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
    = PrimType Int
    | DataType Int
    deriving (Show, Eq, Ord, Generic)
instance Hashable Type

data Entity = Entity
    { entityId :: Int
    , entityLinks :: [Link]
    , entityName :: String
    , entityProps :: [Prop]
    , entityTraits :: [Int]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Entity

data Link = Link
    { linkId :: Int
    , linkEntity :: Int
    , linkName :: String
    , linkTags :: [Int]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Link

data Prop = Prop
    { propId :: Int
    , propName :: String
    , propTags :: [Int]
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
    | EntityLink Int Int
    | EntityName Int String
    | EntityProp Int Int
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
    | TagName Int String
    deriving (Eq, Show)

data SchemacError
    = ParseError ParseError
    | DuplicateData DataName [SourcePos]
    | DuplicateCase DataName CaseName [SourcePos]
    | DuplicateCaseProp DataName CaseName PropName [SourcePos]
    | DuplicateCasePropTag DataName CaseName PropName TagName [SourcePos]
    | DuplicateCaseLink DataName CaseName LinkName [SourcePos]
    | DuplicateCaseLinkTag DataName CaseName LinkName Tag [SourcePos]
    | DuplicateEntity EntityName [SourcePos]
    | DuplicateEntityLink EntityName LinkName [SourcePos]
    | DuplicateEntityProp EntityName PropName [SourcePos]
    | DuplicateEntityPropTag EntityName PropName TagName [SourcePos]
    | DuplicatePrim PrimName [SourcePos]
    | DuplicateTag TagName [SourcePos]
    | UndefinedCasePropTag DataName CaseName PropName TagName [SourcePos]
    | UndefinedCasePropType DataName CaseName PropName TypeName [SourcePos]
    | UndefinedCaseLinkTag DataName CaseName LinkName TagName [SourcePos]
    | UndefinedCaseLinkEntity DataName CaseName LinkName EntityName [SourcePos]
    | UndefinedEntityPropTag EntityName PropName TagName [SourcePos]
    | UndefinedEntityPropType EntityName PropName TypeName [SourcePos]
    | UndefinedEntityLinkTag EntityName LinkName TagName [SourcePos]
    | UndefinedEntityLinkEntity EntityName LinkName EntityName [SourcePos]
    deriving (Show)
    
newtype AST = AST SchemaDeclaration deriving (Show)

type SchemaName = String
type CaseName = String
type DataName = String
type EntityName = String
type LinkName = String
type PrimName = String
type PropName = String
type TagName = String
type TypeName = String

data SchemaDeclaration = SchemaDeclaration SchemaName [SchemaMember] deriving (Show)

data SchemaMember
    = DataDeclaration (DataName, SourcePos) [CaseDeclaration]
    | EntityDeclaration (EntityName, SourcePos) [Field]
    | PrimDeclaration (PrimName, SourcePos)
    | TagDeclaration (TagName, SourcePos)
    deriving (Show)

data CaseDeclaration = CaseDeclaration (CaseName, SourcePos) [Field] deriving (Show)

data Field
    = PropField (PropName, SourcePos) TypeName [TagName]
    | LinkField (LinkName, SourcePos) EntityName [TagName]
    deriving (Show)