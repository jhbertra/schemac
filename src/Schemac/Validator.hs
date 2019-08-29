{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Schemac.Validator
    ( validateAST
    ) where

import Control.Arrow
import Control.Monad

import Data.List (sortBy, groupBy)

import Text.Parsec

import Schemac.Types

import qualified Data.HashMap.Strict as Map

validateAST :: AST -> Either [SchemacError] Schema
validateAST (AST (SchemaDeclaration name members)) = do
    let dataNames = flip concatMap members $ \case { DataDeclaration name _ -> [name]; _ -> [] }
    let entityNames = flip concatMap members $ \case { EntityDeclaration name _ -> [name]; _ -> [] }
    let primNames = flip concatMap members $ \case { PrimDeclaration name -> [name]; _ -> [] }
    let tagNames = flip concatMap members $ \case { TagDeclaration name -> [name]; _ -> [] }

    let cases = concatMap (\case { DataDeclaration (name, _) cases -> [(name, cases)]; _ -> [] }) $ members
    let caseFields = concatMap (\(dataName, cs) -> map (\(CaseDeclaration (caseName, _) fields) -> (dataName, caseName, fields)) cs) cases
    let entityFields = concatMap (\case { EntityDeclaration (name, _) fields -> [(name, fields)]; _ -> [] }) $ members
    
    let caseNames = concatMap (\(dataName, cs) -> map (\(CaseDeclaration (caseName, pos) _)-> (dataName, caseName, pos)) cs) cases
    let casePropNames = concatMap (\(dataName, caseName, fs) -> concatMap (\case { PropField (fieldName, pos) _ _ -> [(dataName, caseName, fieldName, pos)]; _ -> [] }) fs) caseFields
    let caseLinkNames = concatMap (\(dataName, caseName, fs) -> concatMap (\case { LinkField (fieldName, pos) _ _ -> [(dataName, caseName, fieldName, pos)]; _ -> [] }) fs) caseFields
    let entityPropNames = concatMap (\(entityName, fs) -> concatMap (\case { PropField (fieldName, pos) _ _ -> [(entityName, fieldName, pos)]; _ -> [] }) fs) entityFields
    let entityLinkNames = concatMap (\(entityName, fs) -> concatMap (\case { LinkField (fieldName, pos) _ _ -> [(entityName, fieldName, pos)]; _ -> [] }) fs) entityFields

    let duplicateDatas = findDuplicates dataNames
    let duplicateEntities = findDuplicates entityNames
    let duplicatePrims = findDuplicates primNames
    let duplicateTags = findDuplicates tagNames
    let duplicateCases = findDuplicates2 caseNames
    let duplicateCasePropNames = findDuplicates3 casePropNames
    let duplicateCaseLinkNames = findDuplicates3 caseLinkNames
    let duplicateEntityPropNames = findDuplicates2 entityPropNames
    let duplicateEntityLinkNames = findDuplicates2 entityLinkNames
    
    let errors = map (uncurry DuplicateData) duplicateDatas
                <> map (uncurry DuplicateEntity) duplicateEntities
                <> map (uncurry DuplicatePrim) duplicatePrims
                <> map (uncurry DuplicateTag) duplicateTags
                <> map (\((d, c), ps) -> DuplicateCase d c ps) duplicateCases
                <> map (\((d, c, pr), ps) -> DuplicateCaseProp d c pr ps) duplicateCasePropNames
                <> map (\((d, c, l), ps) -> DuplicateCaseLink d c l ps) duplicateCaseLinkNames
                <> map (\((e, pr), ps) -> DuplicateEntityProp e pr ps) duplicateEntityPropNames
                <> map (\((e, l), ps) -> DuplicateEntityLink e l ps) duplicateEntityLinkNames

    Left errors

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
    findDuplicates :: [(String, SourcePos)] -> [(String, [SourcePos])]
    findDuplicates = map (fst . head &&& map snd) . filter ((> 1) . length) . groupBy (\a b -> fst a == fst b) . sortBy (\a b -> fst a `compare` fst b)
    
    findDuplicates2 :: [(String, String, SourcePos)] -> [((String, String), [SourcePos])]
    findDuplicates2 = map (initOf3 . head &&& map lastOf3) . filter ((> 1) . length) . groupBy (\a b -> initOf3 a == initOf3 b) . sortBy (\a b -> initOf3 a `compare` initOf3 b)

    initOf3 (a, b, _) = (a, b)
    lastOf3 (_, _, c) = c

    findDuplicates3 :: [(String, String, String, SourcePos)] -> [((String, String, String), [SourcePos])]
    findDuplicates3 = map (initOf4 . head &&& map lastOf4) . filter ((> 1) . length) . groupBy (\a b -> initOf4 a == initOf4 b) . sortBy (\a b -> initOf4 a `compare` initOf4 b)

    initOf4 (a, b, c, _) = (a, b, c)
    lastOf4 (_, _, _, d) = d

-- getIds
--     :: Member (Input Int) r
--     => [String]
--     -> Sem r (Map.HashMap Int String)
-- getIds = fmap Map.fromList . mapM (\n -> (,n) <$> input)