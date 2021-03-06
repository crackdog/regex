-- | Regex.Regex is a small module to parse a subset of a regular expression into a 
-- haskell datastructure.
module Regex.Regex ( Regex(..),
                     readRegex,
                     regexParser,
                     regexesParser
                   ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<**>))

-- | Regex holds a single regular expression.
data Regex = List [Regex]         -- ^ holds a list of regular expressions, that are evaluated in succession.
           | Class String         -- ^ holds a character class
           | NegateClass String   -- ^ holds an inverted character class
           | Or [Regex]           -- ^ performs an or operation on a list of regular expressions.
           | Min Int Regex        -- ^ Min i r evaluates r at least i times.
           | MinMax Int Int Regex -- ^ MinMax i j r evaluates r at least i and at most j times.
           | Character Char       -- ^ holds a single character
           | AnyCharacter         -- ^ AnyCharacter parses any input character
           deriving Show

-- | readRegex s parses s to a list of Regex or fails with a ParseError
readRegex :: String -> Either ParseError [Regex]
readRegex = parse regexesParser "readRegex"

-- | parseRegex is a Parser for one Regex
regexParser :: Parser Regex
regexParser = choice regexParsers

regexParsers :: [Parser Regex]
regexParsers = map (<**> parseQuantifier) [try parseList,
                                           parseOr,
                                           parseClass,
                                           parseDot,
                                           parseCharacter]

-- | parseRegexes is a Parser for a list of Regex until the end of input.
regexesParser :: Parser [Regex]
regexesParser = manyTill regexParser eof --parseRegex >>= \x -> eof >> return x

parseList :: Parser Regex
parseList = List <$> between (char '(') (char ')') (many1 regexParser)

parseClass :: Parser Regex
parseClass = char '[' >> (parseNegate <*> manyTill anyChar (char ']'))

parseNegate :: Parser (String -> Regex)
parseNegate = (char '^' >> return NegateClass) <|> return Class

parseOr :: Parser Regex
parseOr = Or <$> between (char '(') (char ')') (sepBy1 regexParser (char '|'))

parseQuantifier :: Parser (Regex -> Regex)
parseQuantifier = (char '*' >> return (Min 0))
              <|> (char '+' >> return (Min 1))
              <|> (char '?' >> return (MinMax 0 1))
              <|> parseExplicitQuantifier
              <|> return id

parseExplicitQuantifier :: Parser (Regex -> Regex)
parseExplicitQuantifier = do _ <- char '{'
                             ics <- many1 digit
                             let i = read ics :: Int
                             c <- char '}' <|> char ','
                             case c of
                               '}' -> return $ Min i
                               ',' -> do jcs <- many1 digit
                                         let j = read jcs :: Int
                                         _ <- char '}'
                                         return $ MinMax i j
                               _ -> fail "this should be } or ,"

parseCharacter :: Parser Regex
parseCharacter = Character <$> noneOf "^[.${*(\\+)|?<>"

parseDot :: Parser Regex
parseDot = char '.' >> return AnyCharacter
