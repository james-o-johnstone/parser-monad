module Parser
    ( Parser (..)
    , anyCharBut
    , char
    , letterOrDigit
    , many
    , many1
    , orElse
    , parse
    , runParser
    , sepBy
    ) where

import Data.Maybe
import Data.Char

-- parser extracts a character (a) from the input stream
-- and returns it in a tuple containing the character
-- and the rest of the input stream
data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> (String -> Maybe (a, String))
runParser (P p) = p

-- main entry point to run a parser
-- returns successfully only if parser consumed whole input
-- i.e. if the function inside the Parser a returns a value 
-- of type a along with the empty string.
parse :: Parser a -> String -> Maybe a
parse p string = case runParser p string of
    Just (value, "") -> Just value
    _ -> Nothing

-- noParser represents an always failing parser
noParser :: Parser a
noParser = P (const Nothing)

-- pureParser consumes no input and returns its argument
pureParser :: a -> Parser a
pureParser input = P (\str -> Just (input, str))

-- should have parse (fmap f p) input == fmap f (parse p input) for all f, p, input
instance Functor Parser where
    fmap f p = P p'
        where
            p' str = case runParser p str of
                Just (value, str') -> Just (f value, str')
                _ -> Nothing
          
-- applies the left parser to the input first to get the function. 
-- If it succeeds, it applies the right parser to the remaining input to get the argument, 
-- and returns the function applied to the argument, and the leftover input by the right argument.
instance Applicative Parser where
    -- (<*>) :: f (a -> b) -> f a -> f b
    pure = pureParser
    fp <*> fx = P $ \str -> do
        (func, str') <- runParser fp str
        (value, str'') <- runParser fx str'
        return (func value, str'')

instance Monad Parser where
    return = pureParser
    -- m a -> (a -> m b) -> m b
    fa >>= k = P $ \str -> do
        (value, str') <- runParser fa str
        runParser (k value) str'

    -- bind returns a parser that when given a string does the following:
    -- 1. runs the parser fa on the input
    -- 2. applies a function to the parsed value (k value) which returns a new parser
    -- 3. Runs the new parser on the remaining string

-- anyChar extracts any character from the input stream
-- fails if the input is empty, and takes one character off the input otherwise
anyChar :: Parser Char
anyChar = P $ \str -> case str of
    c:cs -> Just (c, cs)
    [] -> Nothing

char :: Char -> Parser ()
char c = do
    c' <- anyChar -- c' <- anyChar assigns the name c' to the value parsed by anyChar, but is not the result of running "parse anyChar c'"
    if c == c' then return () else noParser -- if the value parsed by anyChar is equal to the input char then this suceeds

-- above desugars to:
-- anyChar >>= \c' -> if c == c' then return () else noParser
-- using definition of bind:
-- anyChar >>= k = \str -> do
-- (value, str') <- runParser fa str
-- runParser (k value) str'
-- where k value == (\c' -> if c == c' then return () else noParser) value
-- which returns a parser (return ()) with value c' (next char in stream) inside if the next character in the input stream matches the character 'c'

anyCharBut :: Char -> Parser Char
anyCharBut c = do
    c' <- anyChar
    if c /= c' then return c' else noParser

-- combinator which tries left parser then tries right parser if left fails
orElse :: Parser a -> Parser a -> Parser a
orElse leftParser rightParser = P $ \str ->
    case runParser leftParser str of
        Nothing -> case runParser rightParser str of
            Just (val, str') -> Just (val, str')
            Nothing -> Nothing
        Just (val, str') -> Just (val, str')

-- applies the parser as often as possible until it fails
-- then returns all results as a list
-- parse (many anyChar) xs = Just xs
-- parse (many noParser) "" = Just []
-- not (null xs) ⇒ parse (many noParser) xs = Nothing
-- -- if no '\n' in xs, then also:
-- parse (many anyCharBut '\n' <* char '\n') (xs++"\n") = Just xs
many :: Parser a -> Parser [a]
many parser = ((:) <$> parser <*> many parser) `orElse` return []

-- many1 that works similar to many but fails if it cannot parse at least once
many1 :: Parser a -> Parser [a]
many1 parser = (:) <$> parser <*> many parser
    
letterOrDigit :: Parser Char
letterOrDigit = do
    c <- anyChar
    if isAlphaNum c then return c else noParser
    
-- so that p1 sepBy p2 applies the p1, then p2, then p1 and so on. 
-- Succeeds if the very first invocation of p1 fails, returning the empty string. 
-- Also succeeds if any invocation of p2 fails, returning the results of all p1 invocations as a list.
sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 >> p1))) `orElse` return []