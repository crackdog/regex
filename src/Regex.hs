-- | Regex implements parsers for a subset of regular expressions.
module Regex ( Regex,
               readRegex,
               checkRegexes,
               checkString,
               checkFullString
             ) where

import Regex.Regex
import Regex.RegexParser
