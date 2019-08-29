{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Schemac
    ( defaultMain
    ) where

import Data.ByteString (readFile)

import Control.Arrow hiding (first)
import Control.Monad

import Data.Bifunctor
import Data.Either
import Data.Hashable
import Data.Maybe

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error

import Prelude hiding (readFile)

import System.Environment

import Schemac.Parser
import Schemac.Types
import Schemac.Validator

data SchemacException
    = LinkEntityNotFound String String
    | LinkTagNotFound String String
    | PropTagNotFound String String
    | PropTypeNotFound String String

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    forM_ args $ \arg -> do
        file <- readFile arg
        print $ (first ((:[]) . ParseError) $ parseSchema arg file) >>= validateAST
-- defaultMain = mapM_ print <=< runM
--     . fmap fst
--     . runOutputList
--     . runInputList
--         [ Schema
--             { schemaId = 0
--             , schemaDatas =
--                 [ Data
--                     5
--                     "PaymentType"
--                     [ Case
--                         6
--                         []
--                         "Cash"
--                         []
--                     , Case
--                         8
--                         []
--                         "CreditCard"
--                         [ Prop 9 "CardNumber" [] $ PrimType 1
--                         , Prop 10 "ExpiryMonth" [] $ PrimType 1
--                         , Prop 11 "ExpiryYear" [] $ PrimType 1
--                         , Prop 12 "CardholderName" [] $ PrimType 7
--                         ]
--                     ]
--                 ]
--             , schemaEntities =
--                 [ Entity
--                     13
--                     [ Link
--                         14
--                         13
--                         "Manager"
--                         [3]
--                     ]
--                     "User"
--                     [ Prop 15 "FirstName" [] $ PrimType 7
--                     , Prop 16 "LastName" [] $ PrimType 7
--                     , Prop 17 "MiddleName" [3] $ PrimType 8
--                     , Prop 18 "PaymentMethods" [3, 4] $ DataType 5
--                     ]
--                     [19]
--                 ]
--             , schemaName = "Test"
--             , schemaPrims = 
--                 [ Prim 1 "int"
--                 , Prim 2 "bool"
--                 , Prim 7 "string"
--                 ]
--             , schemaTags = 
--                 [ Tag 3 "option"
--                 , Tag 4 "many"
--                 ]
--             }
--         ]
--     $ digest

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
    mapM_ (output . DecPrim . primId) schemaPrims
    mapM_ (output . DecProp . propId) (concatMap caseProps $ concatMap dataCases schemaDatas)
    mapM_ (output . DecProp . propId) (concatMap entityProps schemaEntities)
    mapM_ (output . DecTag . tagId) schemaTags

    -- Digest schema members
    output $ SchemaName schemaId schemaName
    mapM_ digestData schemaDatas
    mapM_ digestEntity schemaEntities
    mapM_ (output . uncurry PrimName . (primId &&& primName)) schemaPrims
    mapM_ (output . uncurry TagName . (tagId &&& tagName)) schemaTags

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
    mapM_ digestLink entityLinks
    mapM_ digestProp entityProps

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
    output $ LinkEntity linkId linkEntity
    mapM_ (output . LinkTag linkId) linkTags

digestProp :: Member (Output Emit) r => Prop -> Sem r ()
digestProp Prop{..} = do
    output $ PropName propId propName
    case propType of
        DataType dataId -> output $ PropData propId dataId
        PrimType primId -> output $ PropPrim propId primId
    mapM_ (output . PropTag propId) propTags
