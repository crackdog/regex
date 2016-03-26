module Regex where

import Text.ParserCombinators.Parsec



data Regex = List [Regex]
           | Negate Regex
           | Or [Regex]
           | And [Regex]
           | Many Regex      -- ^ 0 or more
           | Some Regex      -- ^ 1 or more
           | ZeroOrOne Regex
           | Character Char
           | AnyCharacter
           deriving Show

readRegex :: String -> String
readRegex str = case parse parseRegex "regex" str of
                Left err  -> "error: " ++ show err
                Right val -> show val

parseRegex :: Parser Regex
parseRegex = f <$> many parseMultiRegex
  where
    f :: [Regex] -> Regex
    f [x] = x
    f xs  = List xs

parseMultiRegex :: Parser Regex
parseMultiRegex = parseOr
              <|> try parseMany
              <|> try parseSome
              <|> try parseZeroOrOne
              <|> parseAnyCharacter
              <|> parseCharacter

parseSingleRegex :: Parser Regex
parseSingleRegex = parseAnyCharacter <|> parseCharacter

parseList :: Parser Regex
parseList = List <$> many parseRegex

parseOr :: Parser Regex
parseOr = Or <$> between (char '(') (char ')') (sepBy1 parseRegex (char '|'))

followedByChar :: (Regex -> r) -> Char -> Parser r
followedByChar f c = do a <- parseSingleRegex
                        _ <- char c
                        return $ f a

parseMany :: Parser Regex
parseMany = followedByChar Many '*'

parseSome :: Parser Regex
parseSome = followedByChar Some '+'

parseZeroOrOne :: Parser Regex
parseZeroOrOne = followedByChar ZeroOrOne '?'

operators :: String
operators = "|()*+"

parseCharacter :: Parser Regex
parseCharacter = Character <$> noneOf operators

parseAnyCharacter :: Parser Regex
parseAnyCharacter = char '.' >> return AnyCharacter
