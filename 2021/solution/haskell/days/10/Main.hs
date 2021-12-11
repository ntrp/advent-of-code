module Main (main) where

import Common
import Data.List
import Data.Maybe
import Stack as S

-- $setup
-- >>> let testInput = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

open :: Char -> Bool
open = flip elem "([{<"

match :: Maybe Char -> Char -> Bool
match (Just '(') ')' = True
match (Just '[') ']' = True
match (Just '{') '}' = True
match (Just '<') '>' = True
match _ _ = False

-- | detect non matching closing bracket
--
-- >>> detectCorrupted S.empty "([{}<}]"
-- Just '}'
-- >>> detectCorrupted S.empty "([{}<>]"
-- Nothing
detectCorrupted :: Stack Char -> [Char] -> Maybe Char
detectCorrupted _ [] = Nothing
detectCorrupted s (x : xr)
  | open x = detectCorrupted (S.push x s) xr
  | not $ match (S.peek s) x = Just x
  | otherwise = detectCorrupted (snd $ S.pop s) xr

scoreMatcher :: Maybe Char -> Int
scoreMatcher (Just ')') = 3
scoreMatcher (Just ']') = 57
scoreMatcher (Just '}') = 1197
scoreMatcher (Just '>') = 25137
scoreMatcher _ = 0

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "26397"
part1 :: [String] -> String
part1 = show . sum . map (scoreMatcher . detectCorrupted S.empty)

-- |
--
-- >>> computeClosing S.empty "[({(<(())[]>[[{[]{<()<>>"
computeClosing :: Stack Char -> [Char] -> [Char]
computeClosing (Stack s) [] = s
computeClosing s (x : xr)
  | open x = computeClosing (S.push x s) xr
  | otherwise = computeClosing (snd $ S.pop s) xr

flipPar :: Char -> Char
flipPar '(' = ')'
flipPar '[' = ']'
flipPar '{' = '}'
flipPar '<' = '>'
flipPar _ = '^'

scoreMatcherP2 :: Char -> Int
scoreMatcherP2 ')' = 1
scoreMatcherP2 ']' = 2
scoreMatcherP2 '}' = 3
scoreMatcherP2 '>' = 4
scoreMatcherP2 _ = 0

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "288957"
part2 :: [String] -> String
part2 all =
  let incomplete = filter (isNothing . detectCorrupted S.empty) all
      res = sort $ map (foldr (\n acc -> (acc * 5) + n) 0 . reverse . map (scoreMatcherP2 . flipPar) . computeClosing S.empty) incomplete
   in show $ res !! (length res `div` 2)

main :: IO ()
main = interact $ solution part1 part2
