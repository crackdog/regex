module RegexParser where

import Regex
import Control.Monad ( void )
import Text.ParserCombinators.Parsec

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

regex2Parser :: Regex -> Parser ()
regex2Parser = regexes2Parser . return

parseFullString :: Parser a -> Parser a
parseFullString p = do { x <- p; _ <- eof; return x }

test :: [Regex] -> String -> Either ParseError ()
test rs = parse (regexes2Parser rs) "test" 

testRegex :: String -> String -> Either ParseError ()
testRegex rs cs = do regexes <- readRegex rs
                     parse (regexes2Parser regexes) "apply" cs

ca :: Regex
ca = Character 'a'
cb :: Regex
cb = Character 'b'
