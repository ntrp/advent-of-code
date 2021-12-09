module Main (main) where

import Common
import Data.List

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
numberCounter = sum . map (length . filter ((`elem` [2, 3, 4, 7]) . length) . snd)

-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "26"
part1 :: [String] -> String
part1 = show . numberCounter . parseList' dataParser

-- | This is the display description
-- |
-- |  000
-- | 5   1
-- | 5   1
-- |  666
-- | 4   2
-- | 4   2
-- |  333
-- |
-- | be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
-- |
-- | 0 1 2 3 4 5 6
-- |
-- | d b b f f c c
-- |   e e a a g g

-- | find a string in array by length
--
-- >>> findByLength 3 ["1", "22", "4444", "333", "55555"]
-- "333"
findByLength :: Int -> [String] -> String
findByLength i = head . filter ((== i) . length)

-- | remove chars present in another string from the first string
--
-- >>> diff "aefgcdb" "cde"
-- "afgb"
diff :: String -> String -> String
diff s sd = filter (not . flip elem sd) s

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

analyzeConstraints :: DataLine -> [String]
analyzeConstraints dl =
  let all = uncurry (++) dl
      one = findByLength 2 all
      four = findByLength 4 all
      seven = findByLength 3 all
      eight = findByLength 7 all
   in [diff seven one, one, one, diff eight (four ++ seven), diff eight (four ++ seven), diff four one, diff four one]

computePossibleMappings :: [String] -> [String]
computePossibleMappings = filter (not . hasDuplicates) . sequence

decryptDigit :: String -> String -> Either String Int
decryptDigit mapping digit =
  let mapped = map (\x -> if x `elem` digit then '1' else '0') mapping
   in case mapped of
        "1111110" -> Right 0
        "0110000" -> Right 1
        "1101101" -> Right 2
        "1111001" -> Right 3
        "0110011" -> Right 4
        "1011011" -> Right 5
        "1011111" -> Right 6
        "1110000" -> Right 7
        "1111111" -> Right 8
        "1111011" -> Right 9
        _ -> Left "not found"

findMapping :: [String] -> [String] -> Either String String
findMapping [] input = Left "No pattern is valid"
findMapping (m : mx) input =
  case traverse (decryptDigit m) input of
    Right _ -> Right m
    Left _ -> findMapping mx input

decryptDataline :: DataLine -> Either String Int
decryptDataline dl =
  let mapping = flip findMapping (uncurry (++) dl) $ computePossibleMappings $ analyzeConstraints dl
      decrypted = case mapping of
        Right m -> traverse (decryptDigit m) $ snd dl
        Left err -> Left err
   in read . intercalate "" . map show <$> decrypted

-- | should output the correct value give in the test input
--
-- | p.s. horrible code, need to figure this one out....
--
-- >>> part2 $ lines testInput
-- "61229"
part2 :: [String] -> String
part2 = show . sum . fmap sum . map decryptDataline . parseList' dataParser

main :: IO ()
main = interact $ solution part1 part2
