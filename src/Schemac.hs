{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schemac
    ( defaultMain
    ) where

import Data.ByteString (readFile)
import Data.Either

import Control.Arrow hiding (first)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader

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
    { unSchemac :: ExceptT [SchemacException] (StateT ([Emit], Int) IO) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError [SchemacException]
        , MonadState ([Emit], Int)
        , MonadIO
        )

instance MonadEmit Schemac where
    emit e = modify $ _1 %~ (e:)

instance MonadReader Int Schemac where
    ask = _2 <<+= 1
    local f m = do
        (_, i) <- get
        modify $ _2 %~ f
        res <- m
        modify $ _2 %~ const i
        pure res

runSchemac :: MonadIO m => (Emit -> m ()) -> ([SchemacException] -> m b) -> (a -> m b) -> Schemac a -> m b
runSchemac handleEmit runError runSuccess =
    (\(a, (e, _)) -> mapM handleEmit (reverse e) >> either runError runSuccess a)
    <=< liftIO
    . flip runStateT ([], 0)
    . runExceptT
    . unSchemac

defaultMain :: IO ()
defaultMain = runSchemac print print (const $ pure ()) $ do
    args <- liftIO getArgs
    forM args $ \fileName -> digest
        <=< semanticAnalysis
        <=< parseSchema' fileName
        <=< liftIO @Schemac
        $ readFile fileName

  where
    parseSchema' fileName =
        liftEither @_ @Schemac
        . first ((:[]) . SyntaxInvalid)
        . parseSchema fileName