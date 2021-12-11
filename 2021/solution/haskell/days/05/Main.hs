module Main (main) where

import Common
import Data.List
import Parsers

-- $setup
-- >>> let testInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

type X = Int

type Y = Int

data Point = Point X Y deriving (Eq, Ord, Show)

pointX :: Point -> X
pointX (Point x y) = x

pointY :: Point -> Y
pointY (Point x y) = y

point :: Parser Point
point = readPoint <$> many1 digit <* char ',' <*> many1 digit
  where
    readPoint a b = Point (read a) (read b)

data DataLine = DataLine Point Point deriving (Show)

lineStart :: DataLine -> Point
lineStart (DataLine s e) = s

lineEnd :: DataLine -> Point
lineEnd (DataLine s e) = e

line :: Parser DataLine
line = DataLine <$> point <* spaces <* string "->" <* spaces <*> point

-- | parse the data format
--
-- >>> parse' dataParser testInput
-- Right [DataLine (Point 0 9) (Point 5 9),DataLine (Point 8 0) (Point 0 8),DataLine (Point 9 4) (Point 3 4),DataLine (Point 2 2) (Point 2 1),DataLine (Point 7 0) (Point 7 4),DataLine (Point 6 4) (Point 2 0),DataLine (Point 0 9) (Point 2 9),DataLine (Point 3 4) (Point 1 4),DataLine (Point 0 0) (Point 8 8),DataLine (Point 5 5) (Point 8 2)]
dataParser :: Parser [DataLine]
dataParser = sepEndBy1 line eol

isHorV :: DataLine -> Bool
isHorV (DataLine (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

-- | create missing points between two
--
-- >>> generatePointsHV (DataLine (Point 1 2) (Point 1 5))
-- [Point 1 2,Point 1 3,Point 1 4,Point 1 5]
generatePointsHV :: DataLine -> [Point]
generatePointsHV (DataLine (Point x1 y1) (Point x2 y2)) = [Point x y | x <- x1 > x2 ? ([x2 .. x1] :? [x1 .. x2]), y <- y1 > y2 ? ([y2 .. y1] :? [y1 .. y2])]

computeOverlap :: [Point] -> Int
computeOverlap = length . filter ((> 1) . length) . group . sort

--countOccurences :: [Point] ->

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "5"
part1 :: [String] -> String
part1 = show . computeOverlap . concatMap generatePointsHV . filter isHorV . fromRight [] . parse' dataParser . unlines

-- | create missing points between two
--
-- >>> generatePointsD (DataLine (Point 1 2) (Point 3 4))
-- [Point 1 2,Point 2 3,Point 3 4]
-- >>> generatePointsD (DataLine (Point 3 2) (Point 1 4))
-- [Point 3 2,Point 2 3,Point 1 4]
generatePointsD :: DataLine -> [Point]
generatePointsD (DataLine (Point x1 y1) (Point x2 y2)) = zipWith Point (x1 > x2 ? (reverse [x2 .. x1] :? [x1 .. x2])) (y1 > y2 ? (reverse [y2 .. y1] :? [y1 .. y2]))

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "12"
part2 :: [String] -> String
part2 input =
  let parsed = fromRight [] $ parse' dataParser $ unlines input
      hv = concatMap generatePointsHV . filter isHorV $ parsed
      d = concatMap generatePointsD . filter (not . isHorV) $ parsed
   in show $ computeOverlap $ hv ++ d

main :: IO ()
main = interact $ solution part1 part2
