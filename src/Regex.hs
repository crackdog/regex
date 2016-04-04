module Regex ( Regex(..),
               readRegex,
               parseRegex,
               parseRegexes
             ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<**>))

data Regex = List [Regex]
           | Class String
           | NegateClass String
           | Or [Regex]
           | Min Int Regex
           | MinMax Int Int Regex
           | Character Char
           | AnyCharacter
           deriving Show

readRegex :: String -> Either ParseError [Regex]
readRegex = parse parseRegexes "readRegex"

parseRegexes :: Parser [Regex]
parseRegexes = manyTill parseRegex eof --parseRegex >>= \x -> eof >> return x

parseRegex :: Parser Regex
parseRegex = choice regexParsers

regexParsers :: [Parser Regex]
regexParsers = map (<**> parseQunatifier) [try parseList,
                                           parseOr,
                                           parseClass,
                                           parseDot,
                                           parseCharacter]

parseList :: Parser Regex
parseList = List <$> between (char '(') (char ')') (many1 parseRegex)

parseClass :: Parser Regex
parseClass = char '[' >> (parseNegate <*> manyTill anyChar (char ']'))

parseNegate :: Parser (String -> Regex)
parseNegate = (char '^' >> return NegateClass) <|> return Class

parseOr :: Parser Regex
parseOr = Or <$> between (char '(') (char ')') (sepBy1 parseRegex (char '|'))

parseQunatifier :: Parser (Regex -> Regex)
parseQunatifier = (char '*' >> return (Min 0))
              <|> (char '+' >> return (Min 1))
              <|> (char '?' >> return (MinMax 0 1))
              <|> return id

parseCharacter :: Parser Regex
parseCharacter = Character <$> noneOf "^[.${*(\\+)|?<>"

parseDot :: Parser Regex
parseDot = char '.' >> return AnyCharacter
