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

data Trait = Trait
    { traitId :: Int
    , traitLinks :: [Link]
    , traitName :: String
    , traitParents :: [Int]
    , traitProps :: [Prop]
    } deriving (Show, Eq, Ord, Generic)
instance Hashable Trait

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
    | DuplicateEntityTrait EntityName TraitName [SourcePos]
    | DuplicateEntityTraitTag EntityName TraitName Tag [SourcePos]
    | DuplicatePrim PrimName [SourcePos]
    | DuplicateTag TagName [SourcePos]
    | DuplicateTrait TraitName [SourcePos]
    | DuplicateTraitLink TraitName LinkName [SourcePos]
    | DuplicateTraitProp TraitName PropName [SourcePos]
    | DuplicateTraitPropTag TraitName PropName TagName [SourcePos]
    | DuplicateTraitParent TraitName TraitName [SourcePos]
    | UndefinedCasePropTag DataName CaseName PropName TagName [SourcePos]
    | UndefinedCasePropType DataName CaseName PropName TypeName [SourcePos]
    | UndefinedCaseLinkTag DataName CaseName LinkName TagName [SourcePos]
    | UndefinedCaseLinkEntity DataName CaseName LinkName EntityName [SourcePos]
    | UndefinedEntityPropTag EntityName PropName TagName [SourcePos]
    | UndefinedEntityPropType EntityName PropName TypeName [SourcePos]
    | UndefinedEntityLinkTag EntityName LinkName TagName [SourcePos]
    | UndefinedEntityLinkEntity EntityName LinkName EntityName [SourcePos]
    | UndefinedEntityTrait EntityName Trait [SourcePos]
    | UndefinedTraitPropTag TraitName PropName TagName [SourcePos]
    | UndefinedTraitPropType TraitName PropName TypeName [SourcePos]
    | UndefinedTraitLinkTag TraitName LinkName TagName [SourcePos]
    | UndefinedTraitLinkEntity TraitName LinkName EntityName [SourcePos]
    | UndefinedTraitParent TraitName TraitName [SourcePos]
    
newtype AST = AST SchemaDeclaration

type SchemaName = String
type DataName = String
type CaseName = String
type EntityName = String
type LinkName = String
type PrimName = String
type PropName = String
type TagName = String
type TraitName = String
type TypeName = String

data SchemaDeclaration = SchemaDeclaration SchemaName [SchemaMember]

data SchemaMember
    = DataDeclaration DataName [CaseDeclaration]
    | EntityDeclaration EntityName [TraitName] [Field]
    | PrimDeclaration PrimName
    | TagDeclaration TagName
    | TraitDeclaration TraitName [TraitName] [Field]

data CaseDeclaration = CaseDeclaration CaseName [Field]

data Field
    = PropField PropName TypeName [TagName]
    | LinkField LinkName EntityName [TagName]