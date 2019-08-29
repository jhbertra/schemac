module Schemac.Parser
    ( parseSchema
    ) where

import Control.Monad
import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Either
import Data.Functor.Identity

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Indent.Internal

import Schemac.Types

type Parser a = IndentParser ByteString () a

parseSchema :: SourceName -> ByteString -> Either ParseError AST
parseSchema = runIndentParser schemaParser ()

schemaParser :: Parser AST
schemaParser = fmap AST $ SchemaDeclaration
    <$> fmap fst (declaration "schema")
    <*> block memberParser
    <* eof

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum <?> "Identifier"

memberParser :: Parser SchemaMember
memberParser = dataParser
    <|> entityParser
    <|> primParser
    <|> tagParser

dataParser :: Parser SchemaMember
dataParser = 
    (withBlock
        DataDeclaration
        (declaration "data")
        caseParser
    ) <?> "data definition"

entityParser :: Parser SchemaMember
entityParser = 
    (withBlock
        EntityDeclaration
        (declaration "entity")
        fieldParser
    ) <?> "entity definition"

primParser :: Parser SchemaMember
primParser = PrimDeclaration <$> declaration "prim" <?> "prim definition"

tagParser :: Parser SchemaMember
tagParser = TagDeclaration <$> declaration "tag" <?> "tag definition"

caseParser :: Parser CaseDeclaration
caseParser = 
    (withBlock
        CaseDeclaration
        (declaration "case")
        fieldParser
    ) <?> "case definition"

fieldParser :: Parser Field
fieldParser = propParser <|> linkParser

propParser :: Parser Field
propParser = PropField
    <$> declaration "prop"
    <* spaces
    <*> identifier
    <* spaces
    <*> tags

linkParser :: Parser Field
linkParser = LinkField
    <$> declaration "link"
    <* spaces
    <*> identifier
    <*> tags

tags :: Parser [TagName]
tags = many (char '@' *> identifier <* spaces)

declaration :: String -> Parser (String, SourcePos)
declaration keyword = string keyword *> spaces *> (flip (,) <$> statePos `liftM` getParserState <*> identifier) <* spaces <?> keyword ++ " declaration"

newlines :: Parser ()
newlines = void $ many1 newline