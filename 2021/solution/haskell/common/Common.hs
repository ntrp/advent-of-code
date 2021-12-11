module Common
  ( module Common,
    module Control.Applicative,
    module Control.Monad,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
-- Hide a few names that are provided by Applicative.
import Data.Char (isSpace)

notImplemented :: [String] -> String
notImplemented _ = "not implemented"

solution :: ([String] -> String) -> ([String] -> String) -> String -> String
solution part1 part2 content =
  let p1 = part1 $ lines content
      p2 = part2 $ lines content
   in "Part1: " ++ p1 ++ "\nPart2: " ++ p2

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
