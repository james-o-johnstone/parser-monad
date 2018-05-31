module INIParser
    ( parseINI 
    ) where

import Data.Maybe

import Parser

type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile

parseINI = many1 parseSection
    where
        parseSection = do
            identifier <- parseIdentifierLine
            declarations <- many (parseDeclarations `orElse` ignoreLine `orElse` ignoreComment)
            return (identifier, catMaybes declarations)
        parseIdentifierLine = do
            char '['
            identifier <- many1 letterOrDigit
            char ']'
            char '\n'
            return identifier
        parseDeclarations = do
            identifier <- many1 letterOrDigit
            many (anyCharBut '=')
            char '='
            many (char ' ')
            value <- many (anyCharBut '\n')
            char '\n'
            return (Just (identifier, value))
        ignoreLine = do
            char '\n'
            return Nothing
        ignoreComment = do
            char '#'
            many1 (anyCharBut '\n')
            return Nothing