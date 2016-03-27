module Regex ( Regex(..),
               readRegex,
               parseRegex,
               buildParser, test
             ) where

import Text.ParserCombinators.Parsec
import Control.Monad ( void )
import Control.Applicative ((<**>))
import Data.Either ( isRight )

data Regex = List [Regex]
           | Negate Regex
           | Or [Regex]
           | Many Regex      -- ^ 0 or more
           | Some Regex      -- ^ 1 or more
           | ZeroOrOne Regex
           | Character Char
           | AnyCharacter
           deriving Show

readRegex :: String -> String
readRegex str = case parse parseFullRegex "regex" str of
                Left err  -> "error: " ++ show err
                Right val -> show val

parseFullRegex :: Parser Regex
parseFullRegex = parseRegex >>= \x -> eof >> return x

parseRegex :: Parser Regex
parseRegex = f <$> many1 (choice regexParsers)
  where
    f :: [Regex] -> Regex
    f [x] = x
    f xs  = List xs

regexParsers :: [Parser Regex]
regexParsers = map (<**> parseFollowChar) [parseOr,
                                           parseDot,
                                           parseCharacter]

parseList :: Parser Regex
parseList = List <$> many parseRegex

parseOr :: Parser Regex
parseOr = Or <$> between (char '(') (char ')') (sepBy1 parseRegex (char '|'))

parseFollowChar :: Parser (Regex -> Regex)
parseFollowChar = (char '*' >> return Many)
              <|> (char '+' >> return Some)
              <|> (char '?' >> return ZeroOrOne)
              <|> return id

parseCharacter :: Parser Regex
parseCharacter = Character <$> noneOf "^[.${*(\\+)|?<>"

parseDot :: Parser Regex
parseDot = char '.' >> return AnyCharacter

test :: Regex -> String -> Bool
test r = isRight . parse (buildParser r) "test"

buildParser :: Regex -> Parser ()
buildParser (List rs) = buildParser (head rs) --TODO implemenent backtracking
buildParser (Negate r) = fail "not implemented"
buildParser (Or rs) = void . choice $ map buildParser rs
buildParser (Many r) = fail "not implemented"
buildParser (Some r) = fail "not implemented"
buildParser (ZeroOrOne r) = fail "not implemented"
buildParser (Character r) = void $ char r
buildParser AnyCharacter = void anyChar
