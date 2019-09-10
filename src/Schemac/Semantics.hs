{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Schemac.Semantics
    ( semanticAnalysis
    , Prim(..)
    , Tag(..)
    , Case(..)
    , Data(..)
    , Type(..)
    , Entity(..)
    , Link(..)
    , Prop(..)
    , Schema(..)
    , SchemacException(..)
    ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class

import Data.Function
import Data.Hashable
import Data.List (sortBy, groupBy)
import Data.Maybe

import GHC.Generics hiding (to)

import Text.Parsec

import Schemac.Parse

import qualified Data.HashMap.Strict as Map

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

data SchemacException
    = SyntaxInvalid ParseError
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
    | UnresolvedCaseLinkEntity DataName CaseName LinkName EntityName SourcePos
    | UnresolvedCaseLinkTag DataName CaseName LinkName TagName SourcePos
    | UnresolvedCasePropTag DataName CaseName PropName TagName SourcePos
    | UnresolvedCasePropType DataName CaseName PropName TypeName SourcePos
    | UnresolvedEntityLinkEntity EntityName LinkName EntityName SourcePos
    | UnresolvedEntityLinkTag EntityName LinkName TagName SourcePos
    | UnresolvedEntityPropTag EntityName PropName TagName SourcePos
    | UnresolvedEntityPropType EntityName PropName TypeName SourcePos
    deriving (Show)

semanticAnalysis :: MonadError [SchemacException] m => AST -> m Schema
semanticAnalysis (AST (SchemaNode name members)) = do
    let duplicateDatas = findDuplicates . map (view dNodeName &&& view dNodeSourcePos) $ members ^.. each . _Data
    let duplicateEntities = findDuplicates . map (view eNodeName &&& view eNodeSourcePos) $ members ^.. each . _Entity
    let duplicatePrims = findDuplicates . map (view pNodeName &&& view pNodeSourcePos) $ members ^.. each . _Prim
    let duplicateTags = findDuplicates . map (view tNodeName &&& view tNodeSourcePos) $ members ^.. each . _Tag
    let duplicateCases = findDuplicates
                        . concatMap (\d -> map
                            ((const (view dNodeName d) &&& view cNodeName) &&& view cNodeSourcePos)
                            $ d ^.. dNodeCases . each
                        )
                        $ members ^.. each . _Data
    let duplicateCasePropNames = findDuplicates
                        . concatMap (\(d, c) -> map
                            ((const (view dNodeName d) &&& const (view cNodeName c) &&& view prNodeName) &&& view prNodeSourcePos)
                            $ c ^.. cNodeFields . each . _Prop
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data
    let duplicateCaseLinkNames = findDuplicates
                        . concatMap (\(d, c) -> map
                            ((const (view dNodeName d) &&& const (view cNodeName c) &&& view lNodeName) &&& view lNodeSourcePos)
                            $ c ^.. cNodeFields . each . _Link
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data
    let duplicateEntityPropNames = findDuplicates
                        . concatMap (\e -> map
                            ((const (view eNodeName e) &&& view prNodeName) &&& view prNodeSourcePos)
                            $ e ^.. eNodeFields . each . _Prop
                        )
                        $ members ^.. each . _Entity
    let duplicateEntityLinkNames = findDuplicates
                        . concatMap (\e -> map
                            ((const (view eNodeName e) &&& view lNodeName) &&& view lNodeSourcePos)
                            $ e ^.. eNodeFields . each . _Link
                        )
                        $ members ^.. each . _Entity

    let unresolvedCasePropTypes = findUnresolved (members ^.. each . _Prim . pNodeName <> members ^.. each . _Data . dNodeName)
                        . concatMap (\(d, c) -> map
                            (view (prNodeType . tyNodeName)
                                &&& const (view dNodeName d)
                                &&& const (view cNodeName c)
                                &&& view prNodeName
                                &&& view (prNodeType . tyNodeSourcePos)
                            )
                            $ c ^.. cNodeFields . each . _Prop
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data

    let unresolvedCaseLinkEntities = findUnresolved (members ^.. each . _Entity . eNodeName)
                        . concatMap (\(d, c) -> map
                            (view (lNodeType . tyNodeName)
                                &&& const (view dNodeName d)
                                &&& const (view cNodeName c)
                                &&& view lNodeName
                                &&& view (lNodeType . tyNodeSourcePos)
                            )
                            $ c ^.. cNodeFields . each . _Link
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data

    let unresolvedEntityPropTypes = findUnresolved (members ^.. each . _Prim . pNodeName <> members ^.. each . _Data . dNodeName)
                        . concatMap (\e -> map
                            (view (prNodeType . tyNodeName)
                                &&& const (view eNodeName e)
                                &&& view prNodeName
                                &&& view (prNodeType . tyNodeSourcePos)
                            )
                            $ e ^.. eNodeFields . each . _Prop
                        )
                        $ members ^.. each . _Entity

    let unresolvedEntityLinkEntities = findUnresolved (members ^.. each . _Entity . eNodeName)
                        . concatMap (\e -> map
                            (view (lNodeType . tyNodeName)
                                &&& const (view eNodeName e)
                                &&& view lNodeName
                                &&& view (lNodeType . tyNodeSourcePos)
                            )
                            $ e ^.. eNodeFields . each . _Link
                        )
                        $ members ^.. each . _Entity

    let unresolvedCasePropTags = findUnresolved (members ^.. each . _Tag . tNodeName)
                        . concatMap (\(d, (c, p)) -> map
                            (view _1
                                &&& const (view dNodeName d)
                                &&& const (view cNodeName c)
                                &&& const (view prNodeName p)
                                &&& view _2
                            )
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _Prop)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data

    let unresolvedCaseLinkTags = findUnresolved (members ^.. each . _Tag . tNodeName)
                        . concatMap (\(d, (c, l)) -> map
                            (view _1
                                &&& const (view dNodeName d)
                                &&& const (view cNodeName c)
                                &&& const (view lNodeName l)
                                &&& view _2
                            )
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _Link)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _Data

    let unresolvedEntityPropTags = findUnresolved (members ^.. each . _Tag . tNodeName)
                        . concatMap (\(e, p) -> map
                            (view _1
                                &&& const (view eNodeName e)
                                &&& const (view prNodeName p)
                                &&& view _2
                            )
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _Prop)
                        $ members ^.. each . _Entity

    let unresolvedEntityLinkTags = findUnresolved (members ^.. each . _Tag . tNodeName)
                        . concatMap (\(e, l) -> map
                            (view _1
                                &&& const (view eNodeName e)
                                &&& const (view lNodeName l)
                                &&& view _2
                            )
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _Link)
                        $ members ^.. each . _Entity
    
    let errors = map (uncurry DuplicateData) duplicateDatas
                <> map (uncurry DuplicateEntity) duplicateEntities
                <> map (uncurry DuplicatePrim) duplicatePrims
                <> map (uncurry DuplicateTag) duplicateTags
                <> map (\((d, c), ps) -> DuplicateCase d c ps) duplicateCases
                <> map (\((d, (c, pr)), ps) -> DuplicateCaseProp d c pr ps) duplicateCasePropNames
                <> map (\((d, (c, l)), ps) -> DuplicateCaseLink d c l ps) duplicateCaseLinkNames
                <> map (\((e, pr), ps) -> DuplicateEntityProp e pr ps) duplicateEntityPropNames
                <> map (\((e, l), ps) -> DuplicateEntityLink e l ps) duplicateEntityLinkNames
                <> map (\((d, (c, (l, (e, p))))) -> UnresolvedCaseLinkEntity d c l e p) unresolvedCaseLinkEntities
                <> map (\((d, (c, (l, (t, p))))) -> UnresolvedCaseLinkTag d c l t p) unresolvedCaseLinkTags
                <> map (\((d, (c, (pr, (t, p))))) -> UnresolvedCasePropTag d c pr t p) unresolvedCasePropTags
                <> map (\((d, (c, (pr, (t, p))))) -> UnresolvedCasePropType d c pr t p) unresolvedCasePropTypes
                <> map (\((e, (l, (e2, p)))) -> UnresolvedEntityLinkEntity e l e2 p) unresolvedEntityLinkEntities
                <> map (\((e, (l, (t, p)))) -> UnresolvedEntityLinkTag e l t p) unresolvedEntityLinkTags
                <> map (\((e, (pr, (t, p)))) -> UnresolvedEntityPropTag e pr t p) unresolvedEntityPropTags
                <> map (\((e, (pr, (t, p)))) -> UnresolvedEntityPropType e pr t p) unresolvedEntityPropTypes

    -- | UnresolvedEntityLinkEntity EntityName LinkName EntityName SourcePos
    -- | UnresolvedEntityLinkTag EntityName LinkName TagName SourcePos
    -- | UnresolvedEntityPropTag EntityName PropName TagName SourcePos
    -- | UnresolvedEntityPropType EntityName PropName TypeName SourcePos

    throwError errors

    -- schemaId <- input
    -- caseIds <- getIds . map (\(CaseDeclaration name _) -> name) . concatMap (\case { DataDeclaration _ cases -> cases; _ -> [] }) $ members
    -- dataIds <- getIds . dataNames
    -- entityIds <- getIds . entityNames
    -- primIds <- getIds . primNames
    -- tagIds <- getIds . tagNames
    -- let caseFields = concatMap (\(CaseDeclaration _ fields) -> fields) . concatMap (\case { DataDeclaration _ cases -> cases; _ -> [] }) $ members
    -- let entityFields = concatMap (\case { EntityDeclaration _ fields -> fields; _ -> [] }) $ members
    -- let fields = caseFields <> entityFields
    -- propIds <- getIds . concatMap (\case { PropField name _ _ -> [name]; _ -> [] }) $ fields
    -- linkIds <- getIds . concatMap (\case { LinkField name _ _ -> [name]; _ -> [] }) $ fields
    -- pure ()
  where
    findDuplicates :: Ord a => [(a, b)] -> [(a, [b])]
    findDuplicates =
        map (fst . head &&& map snd)
        . filter ((> 1) . length)
        . groupBy ((==) `on` fst)
        . sortBy (compare `on` fst)

    findUnresolved :: [String] -> [(String, a)] -> [(String, a)]
    findUnresolved choices = filter (\a -> all (/= fst a) choices)
-- getIds
--     :: Member (Input Int) r
--     => [String]
--     -> Sem r (Map.HashMap Int String)
-- getIds = fmap Map.fromList . mapM (\n -> (,n) <$> input)