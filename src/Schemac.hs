{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Schemac
    ( defaultMain
    ) where

import Data.ByteString (readFile)
import Data.Either

import Control.Arrow hiding (first)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Bifunctor
import Data.Either
import Data.Hashable
import Data.Maybe

import Prelude hiding (readFile)

import System.Environment

import Schemac.Parse
import Schemac.Semantics
import Schemac.Emit

newtype Schemac a = Schemac
    { unSchemac :: ExceptT [SchemacException] (StateT [Emit] IO) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError [SchemacException]
        , MonadState [Emit]
        , MonadIO
        )

instance MonadEmit Schemac where
    emit e = modify (e:)

runSchemac :: MonadIO m => (Emit -> m ()) -> ([SchemacException] -> m b) -> (a -> m b) -> Schemac a -> m b
runSchemac handleEmit runError runSuccess =
    (\(a, e) -> mapM handleEmit e >> either runError runSuccess a)
    <=< liftIO
    . flip runStateT []
    . runExceptT
    . unSchemac

defaultMain :: IO ()
defaultMain = runSchemac print print print $ do
    args <- liftIO getArgs
    forM args $ \fileName -> semanticAnalysis
        <=< parseSchema' fileName
        <=< liftIO @Schemac
        $ readFile fileName

  where
    parseSchema' fileName =
        liftEither @_ @Schemac
        . first ((:[]) . SyntaxInvalid)
        . parseSchema fileName