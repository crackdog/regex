module RegexParser where

import Regex
import Control.Monad ( void )
import Text.ParserCombinators.Parsec

regexes2Parser :: [Regex] -> Parser ()
regexes2Parser []                  = return ()
regexes2Parser (List rs : rss)     = regexes2Parser rs >> regexes2Parser rss
regexes2Parser (Negate _ : _)      = fail "not implemented"
regexes2Parser (Or rs : rss)       =
    (void . choice $ map regex2Parser rs) >> regexes2Parser rss
regexes2Parser (Many r : rs)       =
    try (regexes2Parser rs) <|> regexes2Parser (Some r : rs)
regexes2Parser (Some r : rs)       =
    do _ <- regex2Parser r
       try (regexes2Parser rs) <|> regexes2Parser (Some r : rs)
regexes2Parser (ZeroOrOne r : rs)  =
    try (regex2Parser r >> regexes2Parser rs) <|> regexes2Parser rs
regexes2Parser (Character r : rs)  = void (char r) >> regexes2Parser rs
regexes2Parser (AnyCharacter : rs) = void anyChar >> regexes2Parser rs

regex2Parser :: Regex -> Parser ()
regex2Parser = regexes2Parser . return

applyParseRegex :: String -> Either ParseError Regex
applyParseRegex = parse parseFullRegex "regex"

testRegex :: String -> String -> Either ParseError ()
testRegex rs cs = do r <- applyParseRegex rs
                     parse (regexes2Parser [r]) "apply" cs
