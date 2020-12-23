module Common (
  module Common,
  module Text.Parsec
) where

import Text.Parsec hiding(count, parse, uncons)
import qualified Text.Parsec as Parsec

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

data Entry = Entry {low :: Int, high :: Int, character :: Char, pass :: String} deriving (Show)

parser :: Parser Entry
parser = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return Entry{low = read low, high = read high, character = c, pass = s}

