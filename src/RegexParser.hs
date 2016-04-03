module RegexParser where

import Regex
--import Data.Either ( isRight )
import Control.Monad ( void )
import Text.ParserCombinators.Parsec

regexes2Parser :: [Regex] -> Parser ()
regexes2Parser []                  = return ()
regexes2Parser (List rs : rss)     = mapM_ regex2Parser rs >> regexes2Parser rss
regexes2Parser (Negate _ : _)      = fail "not implemented"
regexes2Parser (Or rs : rss)       =
    (void . choice $ map regex2Parser rs) >> regexes2Parser rss
regexes2Parser (Many r : rs)       =
    try (regexes2Parser rs) <|> regexes2Parser (Some r : rs)
regexes2Parser (Some r : rs)       = fail "not implemented"
--    do _ <- regex2Parser r
--       try (regexes2Parser rs) <|> regexes2Parser (Some r : rs)
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

checkRegex :: [Regex] -> String -> Either String String
checkRegex []                  "" = Right ""
checkRegex []                  cs = Left cs
checkRegex (_            :_)   "" = Left "reached end of string"
checkRegex (List ls      : rs) cs = checkRegex (ls ++ rs) cs
checkRegex (Negate r     : rs) cs =
    case checkRegex [r] cs of
    Left  newCs -> checkRegex rs newCs
    Right newCs -> Left newCs
checkRegex (Or []        : _)  cs = Left cs
checkRegex (Or (o:os)    : rs) cs =
    case checkRegex [o] cs of
    Left  _     -> checkRegex (Or os : rs) cs
    Right newCs ->
        case checkRegex rs newCs of
        Left  err -> Left err
        Right rcs -> Right rcs
checkRegex (Many r       : rs) cs     =
    case checkRegex (Some r : rs) cs of
    Left  _     -> checkRegex rs cs
    Right ""    -> Right ""
    Right newCs -> Left newCs
checkRegex (Some r       : rs) cs     =
    case checkRegex [r] cs of
    Left  newCs -> Left newCs
    Right newCs ->
        case checkRegex rs newCs of
        Left  _      -> checkRegex (Some r : rs) newCs
        Right ""     -> Right ""
        Right newCs2 -> Left newCs2
checkRegex (ZeroOrOne r  : rs) cs     =
    case checkRegex [r] cs of
    Left  _     -> checkRegex rs cs
    Right newCs -> checkRegex rs newCs
checkRegex (Character r  : rs) (c:cs) | c == r    = checkRegex rs cs
                                      | otherwise = Left (c:cs)
checkRegex (AnyCharacter : rs) (_:cs) = checkRegex rs cs

testCheck :: Regex -> String -> Either String String
testCheck (List ls) = checkRegex ls
testCheck _ = const $ Left "testCheck"

completeTest :: String -> String -> Either String String
completeTest rs = case parse parseFullRegex "regex" rs of
                  Left  _   -> const $ Left "regex parsing error"
                  Right val -> testCheck val

maybeAdd :: Num a => Maybe a -> Maybe a -> Maybe a
maybeAdd a b = do x <- a
                  y <- b
                  return $ x + y

