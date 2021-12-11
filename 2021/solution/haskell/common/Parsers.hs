module Parsers
  ( module Parsers,
    module Text.Parsec.String,
    module Parsec,
  )
where

import Common
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.Matrix
import Debug.Trace as T
import Text.Parsec as Parsec hiding (State, many, optional, (<|>))
import Text.Parsec.String (Parser)

eolChar :: Char
eolChar = '\n'

eol :: Parser Char
eol = char eolChar

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

parseList' ::
  -- | The parser for "a"s
  Parser a ->
  -- | The list of 'String's to parse
  [String] ->
  -- | The resulting list of "a"s
  [a]
parseList' p = either (error . show) id . mapM (parse p "")

whitespace :: Parser String
whitespace = many $ satisfy $ \c -> isSpace c && c /= eolChar

emptyLine :: Parser String
emptyLine = whitespace

emptyLines :: Parser [String]
emptyLines = sepEndBy1 emptyLine eol

println :: (Show a, Monad m) => a -> m ()
println msg = T.trace (show msg) $ return ()

seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

-- | parse multiline digit data to matrix of Int
--
-- >>> fromRight (zero 0 0) $ parse' digitMatrixParser "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
-- ┌                     ┐
-- │ 2 1 9 9 9 4 3 2 1 0 │
-- │ 3 9 8 7 8 9 4 9 2 1 │
-- │ 9 8 5 6 7 8 9 8 9 2 │
-- │ 8 7 6 7 8 9 6 7 8 9 │
-- │ 9 8 9 9 9 6 5 6 7 8 │
-- └                     ┘
digitMatrixParser :: Parser (Matrix Int)
digitMatrixParser = fromLists . map (map $ read . (: [])) <$> sepEndBy1 (many1 digit) eol
