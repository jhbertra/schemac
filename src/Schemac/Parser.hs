{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Schemac.Parser
    ( parseSchema
    ) where

import Data.ByteString
import Data.Either
import Data.Functor.Identity

import Text.Parsec

import Schemac.Types

parseSchema :: SourceName -> ByteString -> Either ParseError AST
parseSchema = runParser schemaParser initState
  where
    initState = ParserState

schemaParser :: Parsec ByteString ParserState AST
schemaParser = 
    fmap AST $ SchemaDeclaration
        <$> declaration "schema"
        <*> many memberParser

identifier :: Parsec ByteString ParserState String
identifier = (:) <$> letter <*> many alphaNum <?> "Identifier"

memberParser :: Parsec ByteString ParserState SchemaMember
memberParser = many1 newline *>
    dataParser
    <|> entityParser
    <|> primParser
    <|> tagParser
    <|> traitParser

dataParser :: Parsec ByteString ParserState SchemaMember
dataParser =
    DataDeclaration
        <$> declaration "data"

entityParser :: Parsec ByteString ParserState SchemaMember
entityParser =
    EntityDeclaration <$> declaration "entity"

primParser :: Parsec ByteString ParserState SchemaMember
primParser = PrimDeclaration <$> declaration "prim"

tagParser :: Parsec ByteString ParserState SchemaMember
tagParser = TagDeclaration <$> declaration "tag"

traitParser :: Parsec ByteString ParserState SchemaMember
traitParser =
    TraitDeclaration <$> declaration "trait"

declaration :: String -> Parsec ByteString ParserState String
declaration keyword = string keyword *> spaces *> identifier


data ParserState = ParserState