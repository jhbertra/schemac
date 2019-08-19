{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Schemac
    ( defaultMain
    ) where

import Control.Arrow
import Control.Monad

import Data.Either
import Data.Hashable
import Data.Maybe

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error

import Schemac.Types

data SchemacException
    = LinkEntityNotFound String String
    | LinkTagNotFound String String
    | PropTagNotFound String String
    | PropTypeNotFound String String

defaultMain :: IO ()
defaultMain = pure ()

digest :: Members '[Input (Maybe Schema), Output Emit] r => Sem r ()
digest = input >>= maybe (pure ()) (\s -> digestSchema s *> digest)
            
digestSchema :: Member (Output Emit) r => Schema -> Sem r ()
digestSchema schema@Schema{..} = do
    -- Declarations
    output $ DecSchema schemaId
    mapM_ (output . DecCase . caseId) (concatMap dataCases schemaDatas)
    mapM_ (output . DecData . dataId) schemaDatas
    mapM_ (output . DecEntity . entityId) schemaEntities
    mapM_ (output . DecLink . linkId) (concatMap caseLinks $ concatMap dataCases schemaDatas)
    mapM_ (output . DecLink . linkId) (concatMap entityLinks schemaEntities)
    mapM_ (output . DecLink . linkId) (concatMap traitLinks schemaTraits)
    mapM_ (output . DecPrim . primId) schemaPrims
    mapM_ (output . DecProp . propId) (concatMap caseProps $ concatMap dataCases schemaDatas)
    mapM_ (output . DecProp . propId) (concatMap entityProps schemaEntities)
    mapM_ (output . DecProp . propId) (concatMap traitProps schemaTraits)
    mapM_ (output . DecTag . tagId) schemaTags
    mapM_ (output . DecTrait . traitId) schemaTraits

    -- Digest schema members
    output $ SchemaName schemaId schemaName
    mapM_ digestData schemaDatas
    mapM_ digestEntity schemaEntities
    mapM_ (output . uncurry PrimName . (primId &&& primName)) schemaPrims
    mapM_ (output . uncurry TagName . (tagId &&& tagName)) schemaTags
    mapM_ digestTrait schemaTraits

    output $ SchemaName schemaId schemaName

digestData :: Member (Output Emit) r => Data -> Sem r ()
digestData Data{..} = do
    output $ DataName dataId dataName
    mapM_ (output . DataCase dataId . caseId) dataCases
    mapM_ digestCase dataCases

digestEntity :: Member (Output Emit) r => Entity -> Sem r ()
digestEntity Entity{..} = do
    output $ EntityName entityId entityName
    mapM_ (output . EntityLink entityId . linkId) entityLinks
    mapM_ (output . EntityProp entityId . propId) entityProps
    mapM_ (output . EntityTrait entityId . traitId) entityTraits
    mapM_ digestLink entityLinks
    mapM_ digestProp entityProps
    mapM_ digestTrait entityTraits

digestTrait :: Member (Output Emit) r => Trait -> Sem r ()
digestTrait Trait{..} = do
    output $ TraitName traitId traitName
    mapM_ (output . TraitLink traitId . linkId) traitLinks
    mapM_ (output . TraitProp traitId . propId) traitProps
    mapM_ digestLink traitLinks
    mapM_ digestProp traitProps

digestCase :: Member (Output Emit) r => Case -> Sem r ()
digestCase Case{..} = do
    output $ CaseName caseId caseName
    mapM_ (output . CaseLink caseId . linkId) caseLinks
    mapM_ (output . CaseProp caseId . propId) caseProps
    mapM_ digestLink caseLinks
    mapM_ digestProp caseProps

digestLink :: Member (Output Emit) r => Link -> Sem r ()
digestLink Link{..} = do
    output $ LinkName linkId linkName
    output $ LinkEntity linkId $ entityId linkEntity
    mapM_ (output . LinkTag linkId . tagId) linkTags

digestProp :: Member (Output Emit) r => Prop -> Sem r ()
digestProp Prop{..} = do
    output $ PropName propId propName
    case propType of
        DataType Data{..} -> output $ PropData propId dataId
        PrimType Prim{..} -> output $ PropPrim propId primId
    mapM_ (output . PropTag propId . tagId) propTags
