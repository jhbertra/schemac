{-# LANGUAGE FlexibleContexts #-}
module Schemac.Emit
    ( Emit(..)
    , MonadEmit(..)
    , digest
    ) where

import Data.ByteString (readFile)
import Data.Either

import Control.Arrow hiding (first)
import Control.Monad

import Data.Hashable
import Data.Maybe

import Prelude hiding (readFile)

import System.Environment

import Schemac.Semantics

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

class (Monad m) => MonadEmit m where
    emit :: Emit -> m ()

digest :: MonadEmit m => Schema -> m ()
digest schema@Schema{..} = do
    -- Declarations
    emit $ DecSchema schemaId
    mapM_ (emit . DecCase . caseId) (concatMap dataCases schemaDatas)
    mapM_ (emit . DecData . dataId) schemaDatas
    mapM_ (emit . DecEntity . entityId) schemaEntities
    mapM_ (emit . DecLink . linkId) (concatMap caseLinks $ concatMap dataCases schemaDatas)
    mapM_ (emit . DecLink . linkId) (concatMap entityLinks schemaEntities)
    mapM_ (emit . DecPrim . primId) schemaPrims
    mapM_ (emit . DecProp . propId) (concatMap caseProps $ concatMap dataCases schemaDatas)
    mapM_ (emit . DecProp . propId) (concatMap entityProps schemaEntities)
    mapM_ (emit . DecTag . tagId) schemaTags

    -- Digest schema members
    emit $ SchemaName schemaId schemaName
    mapM_ digestData schemaDatas
    mapM_ digestEntity schemaEntities
    mapM_ (emit . uncurry PrimName . (primId &&& primName)) schemaPrims
    mapM_ (emit . uncurry TagName . (tagId &&& tagName)) schemaTags

digestData :: MonadEmit m => Data -> m ()
digestData Data{..} = do
    emit $ DataName dataId dataName
    mapM_ (emit . DataCase dataId . caseId) dataCases
    mapM_ digestCase dataCases

digestEntity :: MonadEmit m => Entity -> m ()
digestEntity Entity{..} = do
    emit $ EntityName entityId entityName
    mapM_ (emit . EntityLink entityId . linkId) entityLinks
    mapM_ (emit . EntityProp entityId . propId) entityProps
    mapM_ digestLink entityLinks
    mapM_ digestProp entityProps

digestCase :: MonadEmit m => Case -> m ()
digestCase Case{..} = do
    emit $ CaseName caseId caseName
    mapM_ (emit . CaseLink caseId . linkId) caseLinks
    mapM_ (emit . CaseProp caseId . propId) caseProps
    mapM_ digestLink caseLinks
    mapM_ digestProp caseProps

digestLink :: MonadEmit m => Link -> m ()
digestLink Link{..} = do
    emit $ LinkName linkId linkName
    emit $ LinkEntity linkId linkEntity
    mapM_ (emit . LinkTag linkId) linkTags

digestProp :: MonadEmit m => Prop -> m ()
digestProp Prop{..} = do
    emit $ PropName propId propName
    case propType of
        DataType dataId -> emit $ PropData propId dataId
        PrimType primId -> emit $ PropPrim propId primId
    mapM_ (emit . PropTag propId) propTags
