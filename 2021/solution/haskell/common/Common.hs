module Common
  ( module Common,
    module Control.Applicative,
    module Control.Monad,
    module Parsec,
    module Text.Parsec.String,
    module Debug.Trace,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
-- Hide a few names that are provided by Applicative.
import Data.Char (isSpace)
import Data.Functor.Identity
import Debug.Trace (trace)
import Text.Parsec as Parsec hiding (State, many, optional, (<|>))
import Text.Parsec.String (Parser)

notImplemented :: [String] -> String
notImplemented _ = "not implemented"

solution :: ([String] -> String) -> ([String] -> String) -> String -> String
solution part1 part2 content =
  let p1 = part1 $ lines content
      p2 = part2 $ lines content
   in "Part1: " ++ p1 ++ "\nPart2: " ++ p2

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

println msg = trace (show msg) $ return ()

seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _ = x

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

-- ternary operator
data Cond a = a :? a

infixl 0 ?

infixl 1 :?

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

showEither :: Show a => Show b => Either a b -> String
showEither (Right r) = show r
showEither (Left l) = show l
