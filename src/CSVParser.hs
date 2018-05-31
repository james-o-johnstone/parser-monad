module CSVParser
    ( parseCSV
    ) where

import Parser

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"' -- when given an input this will parse the input if the input = '"' but not do anything with the results, and pass remaining input onwards
        content <- many (anyCharBut '"') -- when given input - parses anychar but '"' and assigns the name "content" to the values parsed by anyChar, continues until failing then passes remaining input onwards
        char '"' -- parses the input if input = '"', doesn't assign the result to anything and passes remaining input onwards
        return content -- wraps content (a list of strings) in the parser monad which has type Parser [[String]]