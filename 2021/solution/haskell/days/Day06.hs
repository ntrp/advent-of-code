module Main(main) where

import Common
import Data.List
import Control.Monad.State

-- $setup
-- >>> let testInput = "3,4,3,1,2"

-- | parse a comma separated int list
--
-- >>> parse' dataParser testInput
-- Right [3,4,3,1,2]
dataParser :: Parser [Int]
dataParser = map read <$> sepBy (many1 digit) (string ",")

type FishValue = String
type FishState = [Int]

-- | do a single tick
--
-- >>> tick [3,4,3,1,2]
-- [2,3,2,0,1]
-- >>> tick [0,1,0,5,6,7,8]
-- [6,0,6,4,5,6,7,8,8]
tick :: [Int] -> [Int]
tick xs = 
    let ready = length . filter (== 0) $ xs
        new = reverse . map (\x -> x > 0 ? x - 1 :? 6) $ xs
     in reverse $ replicate ready 8 ++ new

evolveFish :: Int -> State FishState FishValue 
evolveFish 0 = do
    s <- get
    return $ show . length $ s
evolveFish i = do
    s <- get
    put $ tick s
    evolveFish $ i - 1

evolver :: Int -> Either ParseError [Int] -> FishValue
evolver _ (Left err) = show err
evolver days (Right st) = evalState (evolveFish days) st
    
-- | should output the correct value give in the test input
--
-- >>> part1 $ lines testInput
-- "5934"
part1 :: [String] -> String
part1 = evolver 80 . parse' dataParser . head

type FishStateSmart = [Int]


-- | replace element at i
--
-- >>> replace 3 4 [0,0,0,0,0,0]
-- [0,0,0,4,0,0]
replace :: Int -> Int -> [Int] -> [Int]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- | replace elemts as defined in the input
--
-- >>> setList [(2,7),(4,4)] [0,0,0,0,0,0,0]
-- [0,0,7,0,4,0,0]
setList :: [(Int, Int)] -> [Int] -> [Int]
setList [] xs = xs
setList ((idx, val):idxs) xs = setList idxs $ replace idx val xs 

-- | replace element at i
--
-- >>> add 3 4 [1,2,1,2,5,3]
-- [1,2,1,6,5,3]
add :: Int -> Int -> [Int] -> [Int]
add pos val list = take pos list ++ (val + list !! pos) : drop (pos+1) list

-- | replace elemts as defined in the input
--
-- >>> addList [(2,7),(4,4)] [1,2,1,1,3,1,1]
-- [1,2,8,1,7,1,1]
addList :: [(Int, Int)] -> [Int] -> [Int]
addList [] xs = xs
addList ((idx, val):idxs) xs = addList idxs $ add idx val xs 

evolveFishSmart :: Int -> Int -> State FishStateSmart FishValue 
evolveFishSmart 0 _ = do gets $ show . sum
evolveFishSmart d i = do
    s <- get
    let newBorns = s !! i
    let indexes = case newBorns of
                    0 -> []
                    val -> map (\idx -> (idx, val)) $ [x * 7 + i + 2 | x <- [1..d], x * 7 + i + 2 < d + i]
    put $ addList indexes s
    evolveFishSmart (d - 1) (i + 1)

evolverSmart :: Int -> Either ParseError [Int] -> FishValue
evolverSmart _ (Left err) = show err
evolverSmart days (Right st) = 
    let init = replicate days 0
        indexes = map (\xs -> (head xs, length xs)) . group . sort . concatMap (\i -> [x * 7 + i | x <- [0..days], x * 7 + i < days]) $ st
    in show $ (length st) + (read $ evalState (evolveFishSmart days 0) (setList indexes init))

-- | should output the correct value give in the test input
--
-- >>> part2 $ lines testInput
-- "26984457539"
part2 :: [String] -> String
part2 = evolverSmart 256 . parse' dataParser . head

main :: IO ()
main = interact $ solution part1 part2
