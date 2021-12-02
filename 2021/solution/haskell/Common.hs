module Common (
  module Common,
  module Text.Parsec
) where

import Text.Parsec hiding(count, parse, uncons)
import qualified Text.Parsec as Parsec

solution :: ([String] -> String) -> ([String] -> String) -> String -> String
solution part1 part2 content =
    let p1 = part1 $ lines content
        p2 = part2 $ lines content
     in "Part1: " ++ p1 ++ "\nPart2: " ++ p2

-- | 'Parser' is a convenience type for 'Parsec'
type Parser = Parsec String ()

-- | The 'parse' function is a convenience function for 'Parsec.parse' that
-- removes the requirement to provide a file name.
parse :: Parser a            -- ^ The parser for "a"s
      -> String              -- ^ The string to be parsed
      -> Either ParseError a -- ^ The successfully parsed value or an error
parse p = Parsec.parse p ""

-- | The 'parselist' function parses a list of 'String's using 'parse' and
-- returns the list of parsed "a"s.  If any parse was unsuccessful we crash the
-- program, showing the first error encountered.
parselist :: Parser a -- ^ The parser for "a"s
          -> [String] -- ^ The list of 'String's to parse
          -> [a]      -- ^ The resulting list of "a"s
parselist p = either (error . show) id . mapM (parse p)

