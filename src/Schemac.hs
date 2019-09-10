{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schemac
    ( defaultMain
    ) where

import Control.Arrow hiding (first)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader

import Data.Bifunctor
import Data.ByteString (readFile)
import Data.Either

import Prelude hiding (readFile)

import System.Environment
import System.Exit
import System.IO hiding (readFile)

import Text.Parsec

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
defaultMain = runSchemac print (mapM_ printError >=> const exitFailure) (const $ pure ()) $ do
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
    
    printError :: SchemacException -> IO ()
    printError e =
        forM_ (paths e) $ \(path, pos) -> do
            hPutStr stderr $ "ERROR SC" <> lPad 4 '0' (show $ errorCode e)
            hPutStr stderr $ ": " <> message e
            hPutStrLn stderr $ " - " <> show pos
            forM_ path $ \node -> hPutStrLn stderr $ "    in: " <> node


    paths :: SchemacException -> [([String], SourcePos)]
    paths (SyntaxInvalid e) = [([], errorPos e)]
    paths (DuplicateData _ ps) = map (const [] &&& id) ps
    paths (DuplicateCase dName _ ps) = map (const ["data " <> dName] &&& id) ps
    paths (DuplicateCaseProp dName cName _ ps) = map (const ["data " <> dName, "case " <> cName] &&& id) ps
    paths (DuplicateCasePropTag dName cName pName _ ps) = map (const ["data " <> dName, "case " <> cName, "prop " <> pName] &&& id) ps
    paths (DuplicateCaseLink dName cName _ ps) = map (const ["data " <> dName, "case " <> cName] &&& id) ps
    paths (DuplicateCaseLinkTag dName cName lName _ ps) = map (const ["data " <> dName, "case " <> cName, "link " <> lName] &&& id) ps
    paths (DuplicateEntity _ ps) = map (const [] &&& id) ps
    paths (DuplicateEntityLink eName _ ps) = map (const ["entity " <> eName] &&& id) ps
    paths (DuplicateEntityLinkTag eName lName _ ps) = map (const ["entity " <> eName, "link " <> lName] &&& id) ps
    paths (DuplicateEntityProp eName _ ps) = map (const ["entity " <> eName] &&& id) ps
    paths (DuplicateEntityPropTag eName pName _ ps) = map (const ["entity " <> eName, "prop " <> pName] &&& id) ps
    paths (DuplicatePrim _ ps) = map (const [] &&& id) ps
    paths (DuplicateTag _ ps) = map (const [] &&& id) ps
    paths (UnresolvedCaseLinkEntity d c l _ p) = [(["data " <> d, "case " <> c, "link " <> l], p)]
    paths (UnresolvedCaseLinkTag d c l _ p) = [(["data " <> d, "case " <> c, "link " <> l], p)]
    paths (UnresolvedCasePropTag d c pr _ p) = [(["data " <> d, "case " <> c, "prop " <> pr], p)]
    paths (UnresolvedCasePropType d c pr _ p) = [(["data " <> d, "case " <> c, "prop " <> pr], p)]
    paths (UnresolvedEntityLinkEntity e l _ p) = [(["entity " <> e, "link " <> l], p)]
    paths (UnresolvedEntityLinkTag e l _ p) = [(["entity " <> e, "link " <> l], p)]
    paths (UnresolvedEntityPropTag e pr _ p) = [(["entity " <> e, "prop " <> pr], p)]
    paths (UnresolvedEntityPropType e pr _ p) = [(["entity " <> e, "prop " <> pr], p)]
    
    message :: SchemacException -> String
    message (SyntaxInvalid _) = "Syntax error"
    message (DuplicateData dName _) = "Duplicate declaration \"data " <> dName <> "\""
    message (DuplicateCase _ cName _) = "Duplicate case declaration \"case " <> cName <> "\""
    message (DuplicateCaseProp _ _ pName _) = "Duplicate prop declaration \"prop " <> pName <> "\""
    message (DuplicateCasePropTag _ _ _ tName _) = "Duplicate tag \"#" <> tName <> "\""
    message (DuplicateCaseLink _ _ lName _) = "Duplicate link declaration \"link " <> lName <> "\""
    message (DuplicateCaseLinkTag _ _ _ tName _) = "Duplicate tag \"#" <> tName <> "\""
    message (DuplicateEntity eName _) = "Duplicate entity declaration \"entity " <> eName <> "\""
    message (DuplicateEntityLink _ lName _) = "Duplicate link declaration \"link " <> lName <> "\""
    message (DuplicateEntityLinkTag _ _ tName _) = "Duplicate tag \"#" <> tName <> "\""
    message (DuplicateEntityProp _ pName _) = "Duplicate prop declaration \"prop " <> pName <> "\""
    message (DuplicateEntityPropTag _ _ tName _) = "Duplicate tag \"#" <> tName <> "\""
    message (DuplicatePrim pName _) = "Duplicate prim declaration \"prim " <> pName <> "\""
    message (DuplicateTag tName _) = "Duplicate tag declaration \"tag " <> tName <> "\""
    message (UnresolvedCaseLinkEntity _ _ _ eName _) = "Unresolved entity name \"" <> eName <> "\""
    message (UnresolvedCaseLinkTag _ _ _ tName _) = "Unresolved tag \"#" <> tName <> "\""
    message (UnresolvedCasePropTag _ _ _ tName _) = "Unresolved tag \"#" <> tName <> "\""
    message (UnresolvedCasePropType _ _ _ tyName _) = "Unresolved data or prim name \"" <> tyName <> "\""
    message (UnresolvedEntityLinkEntity _ _ eName _) = "Unresolved entity name \"" <> eName <> "\""
    message (UnresolvedEntityLinkTag _ _ tName _) = "Unresolved tag \"#" <> tName <> "\""
    message (UnresolvedEntityPropTag _ _ tName _) = "Unresolved tag \"#" <> tName <> "\""
    message (UnresolvedEntityPropType _ _ tyName _) = "Unresolved data or prim name \"" <> tyName <> "\""

    errorCode :: SchemacException -> Int
    errorCode (SyntaxInvalid _) = 1
    errorCode (DuplicateData _ _) = 2
    errorCode (DuplicateCase _ _ _) = 3
    errorCode (DuplicateCaseProp _ _ _ _) = 4
    errorCode (DuplicateCasePropTag _ _ _ _ _) = 5
    errorCode (DuplicateCaseLink _ _ _ _) = 6
    errorCode (DuplicateCaseLinkTag _ _ _ _ _) = 7
    errorCode (DuplicateEntity _ _) = 8
    errorCode (DuplicateEntityLink _ _ _) = 6
    errorCode (DuplicateEntityLinkTag _ _ _ _) = 7
    errorCode (DuplicateEntityProp _ _ _) = 4
    errorCode (DuplicateEntityPropTag _ _ _ _) = 5
    errorCode (DuplicatePrim _ _) = 9
    errorCode (DuplicateTag _ _) = 10
    errorCode (UnresolvedCaseLinkEntity _ _ _ _ _) = 11
    errorCode (UnresolvedCaseLinkTag _ _ _ _ _) = 12
    errorCode (UnresolvedCasePropTag _ _ _ _ _) = 12
    errorCode (UnresolvedCasePropType _ _ _ _ _) = 13
    errorCode (UnresolvedEntityLinkEntity _ _ _ _) = 11
    errorCode (UnresolvedEntityLinkTag _ _ _ _) = 12
    errorCode (UnresolvedEntityPropTag _ _ _ _) = 12
    errorCode (UnresolvedEntityPropType _ _ _ _) = 13

lPad :: Int -> a -> [a] -> [a]
lPad m a xs = replicate (m - length ys) a <> ys
    where ys = take m xs