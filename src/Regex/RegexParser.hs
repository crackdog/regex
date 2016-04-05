-- | Regex.RegexParser contains Parsers for Regexes.
module Regex.RegexParser ( regexes2Parser,
                           checkRegexes,
                           checkString,
                           checkFullString
                         ) where

import Regex.Regex
import Control.Monad ( void )
import Text.ParserCombinators.Parsec

-- | regexes2Parser rs builds a Parser from rs.
regexes2Parser :: [Regex] -> Parser ()
regexes2Parser []                     = return ()
regexes2Parser (List rs : rss)        = regexes2Parser rs >> regexes2Parser rss
regexes2Parser (Class rs : rss)       = oneOf rs >> regexes2Parser rss
regexes2Parser (NegateClass rs : rss) = noneOf rs >> regexes2Parser rss
regexes2Parser (Or rs : rss)          =
    (void . choice $ map regex2Parser rs) >> regexes2Parser rss
regexes2Parser (Min i r : rs)
  | i <= 0    = try (regexes2Parser rs) <|> regexes2Parser (Min 1 r :rs)
  | otherwise = regex2Parser r >> regexes2Parser (Min (i-1) r : rs)
regexes2Parser (MinMax i j r : rs)
  | j <= 0    = regexes2Parser rs
  | i <= 0    = try (regexes2Parser rs) 
            <|> (regex2Parser r >> regexes2Parser (MinMax 0 (j-1) r : rs))
  | otherwise = regex2Parser r >> regexes2Parser (MinMax (i-1) (j-1) r : rs)
regexes2Parser (Character r : rs)     = void (char r) >> regexes2Parser rs
regexes2Parser (AnyCharacter : rs)    = void anyChar >> regexes2Parser rs

-- | regex2Parser r builds a Parser for the single Regex r.
regex2Parser :: Regex -> Parser ()
regex2Parser = regexes2Parser . return

-- | parseFullString p fist applies p and then eof.
parseFullString :: Parser a -> Parser a
parseFullString p = do { x <- p; _ <- eof; return x }

-- | checkRegexes rs str builds the Parser for rs and then parses str.
checkRegexes :: [Regex] -> String -> Either ParseError ()
checkRegexes rs = parse (regexes2Parser rs) "apply regex" 

-- | checkString rs str builds a Parser from rs and then parsers str.
checkString :: String -> String -> Either ParseError ()
checkString rs cs = do regexes <- readRegex rs
                       parse (regexes2Parser regexes) "apply regex" cs

-- | As checkString, but fails if the regex doesn't apply to the hole string.
checkFullString :: String -> String -> Either ParseError ()
checkFullString rs cs = do regexes <- readRegex rs
                           parse (parseFullString (regexes2Parser regexes))
                                 "apply regex" cs
