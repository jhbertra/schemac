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
import Control.Monad.Error.Class
import Control.Monad.Reader

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
    | DuplicateCaseLinkTag DataName CaseName LinkName TagName [SourcePos]
    | DuplicateEntity EntityName [SourcePos]
    | DuplicateEntityLink EntityName LinkName [SourcePos]
    | DuplicateEntityLinkTag EntityName LinkName TagName [SourcePos]
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

semanticAnalysis
    :: (MonadError [SchemacException] m, MonadReader Int m)
    => AST
    -> m Schema
semanticAnalysis (AST (SchemaNode name members)) = do
    let duplicateDatas = findDuplicates . map (_dNodeName &&& _dNodeSourcePos) $ members ^.. each . _DataMember
    let duplicateEntities = findDuplicates . map (_eNodeName &&& _eNodeSourcePos) $ members ^.. each . _EntityMember
    let duplicatePrims = findDuplicates . map (_pNodeName &&& _pNodeSourcePos) $ members ^.. each . _PrimMember
    let duplicateTags = findDuplicates . map (_tNodeName &&& _tNodeSourcePos) $ members ^.. each . _TagMember
    let duplicateCases = findDuplicates
                        . concatMap (\d -> map
                            ((const (_dNodeName d) &&& _cNodeName) &&& _cNodeSourcePos)
                            $ d ^.. dNodeCases . each
                        )
                        $ members ^.. each . _DataMember
    let duplicateCasePropNames = findDuplicates
                        . concatMap (\(d, c) -> map
                            ((const (_dNodeName d) &&& const (_cNodeName c) &&& _prNodeName) &&& _prNodeSourcePos)
                            $ c ^.. cNodeFields . each . _PropField
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember
    let duplicateCasePropTags = findDuplicates
                        . concatMap (\(d, (c, p)) -> map
                            ((const (_dNodeName d) &&& const (_cNodeName c) &&& const (_prNodeName p) &&& fst) &&& snd)
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _PropField)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember
    let duplicateCaseLinkNames = findDuplicates
                        . concatMap (\(d, c) -> map
                            ((const (_dNodeName d) &&& const (_cNodeName c) &&& _lNodeName) &&& _lNodeSourcePos)
                            $ c ^.. cNodeFields . each . _LinkField
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember
    let duplicateCaseLinkTags = findDuplicates
                        . concatMap (\(d, (c, l)) -> map
                            ((const (_dNodeName d) &&& const (_cNodeName c) &&& const (_lNodeName l) &&& fst) &&& snd)
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _LinkField)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember
    let duplicateEntityPropNames = findDuplicates
                        . concatMap (\e -> map
                            ((const (_eNodeName e) &&& _prNodeName) &&& _prNodeSourcePos)
                            $ e ^.. eNodeFields . each . _PropField
                        )
                        $ members ^.. each . _EntityMember
    let duplicateEntityPropTags = findDuplicates
                        . concatMap (\(e, p) -> map
                            ((const (_eNodeName e) &&& const (_prNodeName p) &&& fst) &&& snd)
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _PropField)
                        $ members ^.. each . _EntityMember
    let duplicateEntityLinkNames = findDuplicates
                        . concatMap (\e -> map
                            ((const (_eNodeName e) &&& _lNodeName) &&& _lNodeSourcePos)
                            $ e ^.. eNodeFields . each . _LinkField
                        )
                        $ members ^.. each . _EntityMember
    let duplicateEntityLinkTags = findDuplicates
                        . concatMap (\(e, l) -> map
                            ((const (_eNodeName e) &&& const (_lNodeName l) &&& fst) &&& snd)
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _LinkField)
                        $ members ^.. each . _EntityMember

    let unresolvedCasePropTypes = findUnresolved (members ^.. each . _PrimMember . pNodeName <> members ^.. each . _DataMember . dNodeName)
                        . concatMap (\(d, c) -> map
                            (view (prNodeType . tyNodeName)
                                &&& const (_dNodeName d)
                                &&& const (_cNodeName c)
                                &&& _prNodeName
                                &&& view (prNodeType . tyNodeSourcePos)
                            )
                            $ c ^.. cNodeFields . each . _PropField
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember

    let unresolvedCaseLinkEntities = findUnresolved (members ^.. each . _EntityMember . eNodeName)
                        . concatMap (\(d, c) -> map
                            (view (lNodeType . tyNodeName)
                                &&& const (_dNodeName d)
                                &&& const (_cNodeName c)
                                &&& _lNodeName
                                &&& view (lNodeType . tyNodeSourcePos)
                            )
                            $ c ^.. cNodeFields . each . _LinkField
                        )
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember

    let unresolvedEntityPropTypes = findUnresolved (members ^.. each . _PrimMember . pNodeName <> members ^.. each . _DataMember . dNodeName)
                        . concatMap (\e -> map
                            (view (prNodeType . tyNodeName)
                                &&& const (_eNodeName e)
                                &&& _prNodeName
                                &&& view (prNodeType . tyNodeSourcePos)
                            )
                            $ e ^.. eNodeFields . each . _PropField
                        )
                        $ members ^.. each . _EntityMember

    let unresolvedEntityLinkEntities = findUnresolved (members ^.. each . _EntityMember . eNodeName)
                        . concatMap (\e -> map
                            (view (lNodeType . tyNodeName)
                                &&& const (_eNodeName e)
                                &&& _lNodeName
                                &&& view (lNodeType . tyNodeSourcePos)
                            )
                            $ e ^.. eNodeFields . each . _LinkField
                        )
                        $ members ^.. each . _EntityMember

    let unresolvedCasePropTags = findUnresolved (members ^.. each . _TagMember . tNodeName)
                        . concatMap (\(d, (c, p)) -> map
                            (fst
                                &&& const (_dNodeName d)
                                &&& const (_cNodeName c)
                                &&& const (_prNodeName p)
                                &&& snd
                            )
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _PropField)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember

    let unresolvedCaseLinkTags = findUnresolved (members ^.. each . _TagMember . tNodeName)
                        . concatMap (\(d, (c, l)) -> map
                            (fst
                                &&& const (_dNodeName d)
                                &&& const (_cNodeName c)
                                &&& const (_lNodeName l)
                                &&& snd
                            )
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\(d, c) -> map (const d &&& const c &&& id) $ c ^.. cNodeFields . each . _LinkField)
                        . concatMap (\d -> map (const d &&& id) $ d ^.. dNodeCases . each)
                        $ members ^.. each . _DataMember

    let unresolvedEntityPropTags = findUnresolved (members ^.. each . _TagMember . tNodeName)
                        . concatMap (\(e, p) -> map
                            (fst
                                &&& const (_eNodeName e)
                                &&& const (_prNodeName p)
                                &&& snd
                            )
                            $ p ^.. prNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _PropField)
                        $ members ^.. each . _EntityMember

    let unresolvedEntityLinkTags = findUnresolved (members ^.. each . _TagMember . tNodeName)
                        . concatMap (\(e, l) -> map
                            (fst
                                &&& const (_eNodeName e)
                                &&& const (_lNodeName l)
                                &&& snd
                            )
                            $ l ^.. lNodeTags . each
                        )
                        . concatMap (\e -> map (const e &&& id) $ e ^.. eNodeFields . each . _LinkField)
                        $ members ^.. each . _EntityMember
    
    let errors = map (uncurry DuplicateData) duplicateDatas
                <> map (uncurry DuplicateEntity) duplicateEntities
                <> map (uncurry DuplicatePrim) duplicatePrims
                <> map (uncurry DuplicateTag) duplicateTags
                <> map (\((d, c), ps) -> DuplicateCase d c ps) duplicateCases
                <> map (\((d, (c, pr)), ps) -> DuplicateCaseProp d c pr ps) duplicateCasePropNames
                <> map (\((d, (c, (p, t))), ps) -> DuplicateCasePropTag d c p t ps) duplicateCasePropTags
                <> map (\((d, (c, l)), ps) -> DuplicateCaseLink d c l ps) duplicateCaseLinkNames
                <> map (\((d, (c, (l, t))), ps) -> DuplicateCaseLinkTag d c l t ps) duplicateCaseLinkTags
                <> map (\((e, pr), ps) -> DuplicateEntityProp e pr ps) duplicateEntityPropNames
                <> map (\((e, (p, t)), ps) -> DuplicateEntityPropTag e p t ps) duplicateEntityPropTags
                <> map (\((e, l), ps) -> DuplicateEntityLink e l ps) duplicateEntityLinkNames
                <> map (\((e, (l, t)), ps) -> DuplicateEntityLinkTag e l t ps) duplicateEntityLinkTags
                <> map (\((e, (d, (c, (l, p))))) -> UnresolvedCaseLinkEntity d c l e p) unresolvedCaseLinkEntities
                <> map (\((t, (d, (c, (l, p))))) -> UnresolvedCaseLinkTag d c l t p) unresolvedCaseLinkTags
                <> map (\((t, (d, (c, (pr, p))))) -> UnresolvedCasePropTag d c pr t p) unresolvedCasePropTags
                <> map (\((t, (d, (c, (pr, p))))) -> UnresolvedCasePropType d c pr t p) unresolvedCasePropTypes
                <> map (\((e2, (e, (l, p)))) -> UnresolvedEntityLinkEntity e l e2 p) unresolvedEntityLinkEntities
                <> map (\((t, (e, (l, p)))) -> UnresolvedEntityLinkTag e l t p) unresolvedEntityLinkTags
                <> map (\((t, (e, (pr, p)))) -> UnresolvedEntityPropTag e pr t p) unresolvedEntityPropTags
                <> map (\((t, (e, (pr, p)))) -> UnresolvedEntityPropType e pr t p) unresolvedEntityPropTypes

    if not $ null errors
        then throwError errors
        else do
            schemaId <- ask
            caseIds <- getIds $ members ^.. each . _DataMember . dNodeCases . each . cNodeName
            dataIds <- getIds $ members ^.. each . _DataMember . dNodeName
            entityIds <- getIds $ members ^.. each . _EntityMember . eNodeName
            primIds <- getIds $ members ^.. each . _PrimMember . pNodeName
            tagIds <- getIds $ members ^.. each . _TagMember . tNodeName
            let caseFields = members ^.. each . _DataMember . dNodeCases . each . cNodeFields . each
            let entityFields = members ^.. each . _EntityMember . eNodeFields . each
            let fields = caseFields <> entityFields
            propIds <- getIds $ fields ^.. each . _PropField . prNodeName
            linkIds <- getIds $ fields ^.. each . _LinkField . lNodeName
            
            pure $ Schema
                schemaId
                (members
                    ^.. each
                    . _DataMember
                    . to
                        (\DataNode{..} -> Data
                            (dataIds `unsafeLookup` _dNodeName)
                            _dNodeName
                            $ _dNodeCases
                                ^.. each
                                . to
                                    (\CaseNode{..} -> Case
                                        (caseIds `unsafeLookup` _cNodeName)
                                        (_cNodeFields
                                            ^.. each
                                            . _LinkField
                                            . to
                                                (\LinkNode{..} -> Link
                                                    (linkIds `unsafeLookup` _lNodeName)
                                                    (unsafeLookup entityIds . _tyNodeName $ _lNodeType)
                                                    _lNodeName
                                                    $ map (unsafeLookup tagIds . fst) _lNodeTags
                                                )
                                        )
                                        _cNodeName
                                        $ _cNodeFields
                                            ^.. each
                                            . _PropField
                                            . to
                                                (\PropNode{..} -> Prop
                                                    (propIds `unsafeLookup` _prNodeName)
                                                    _prNodeName
                                                    (map (unsafeLookup tagIds . fst) _prNodeTags)
                                                    (fromMaybe
                                                        (DataType . unsafeLookup dataIds $ _tyNodeName _prNodeType)
                                                        $ PrimType <$> Map.lookup (_tyNodeName _prNodeType) primIds
                                                    )
                                                )
                                    )        
                        )
                )
                (members
                    ^.. each
                    . _EntityMember
                    . to
                        (\EntityNode{..} -> Entity
                            (entityIds `unsafeLookup` _eNodeName)
                            (_eNodeFields
                                ^.. each
                                . _LinkField
                                . to
                                    (\LinkNode{..} -> Link
                                        (linkIds `unsafeLookup` _lNodeName)
                                        (unsafeLookup entityIds . _tyNodeName $ _lNodeType)
                                        _lNodeName
                                        $ map (unsafeLookup tagIds . fst) _lNodeTags
                                    )
                            )
                            _eNodeName
                            $ _eNodeFields
                                ^.. each
                                . _PropField
                                . to
                                    (\PropNode{..} -> Prop
                                        (propIds `unsafeLookup` _prNodeName)
                                        _prNodeName
                                        (map (unsafeLookup tagIds . fst) _prNodeTags)
                                        (fromMaybe
                                            (DataType . unsafeLookup dataIds $ _tyNodeName _prNodeType)
                                            $ PrimType <$> Map.lookup (_tyNodeName _prNodeType) primIds
                                        )
                                    )
                        )
                )
                name
                (members ^.. each . _PrimMember . to (\PrimNode{..} -> Prim (primIds `unsafeLookup` _pNodeName) _pNodeName))
                $ members ^.. each . _TagMember . to (\TagNode{..} -> Tag (tagIds `unsafeLookup` _tNodeName) _tNodeName)
  where
    findDuplicates :: Ord a => [(a, b)] -> [(a, [b])]
    findDuplicates =
        map (fst . head &&& map snd)
        . filter ((> 1) . length)
        . groupBy ((==) `on` fst)
        . sortBy (compare `on` fst)

    findUnresolved :: [String] -> [(String, a)] -> [(String, a)]
    findUnresolved choices = filter (\a -> all (/= fst a) choices)
    
    getIds :: MonadReader Int m => [String] -> m (Map.HashMap String Int)
    getIds = fmap Map.fromList . mapM (\n -> (n,) <$> ask)

    unsafeLookup :: (Hashable a, Eq a) => Map.HashMap a b -> a -> b
    unsafeLookup hMap a = case Map.lookup a hMap of Just b -> b