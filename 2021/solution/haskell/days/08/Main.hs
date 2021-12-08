module Main (main) where

import Common

-- $setup
-- >>> let testInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"

type DataLine = ([String], [String])

segments :: Parser [String]
segments = words <$> many1 (oneOf ['a' .. 'z'] <|> char ' ')

-- | parse one data line
--
-- >>> parse' dataParser "ac c gcdf | cb f cdfeb"
-- Right (["ac","c","gcdf"],["cb","f","cdfeb"])
dataParser :: Parser DataLine
dataParser = (,) <$> segments <* char '|' <*> segments

-- | find words matching the length
--
-- >>> numberCounter [(["ac","c","gcdf"],["cb","f","cdf", "abcde"])]
-- 2
numberCounter :: [DataLine] -> Int
numberCounter = sum . map (length . filter((flip elem [2,3,4,7]) . length) . snd)

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "26"
part1 :: [String] -> String
part1 = show . numberCounter . parseList' dataParser

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "not implemented"
part2 :: [String] -> String
part2 = notImplemented

main :: IO ()
main = interact $ solution part1 part2
