module Main(main) where

import Common
import Control.Monad.State

-- $setup
-- >>> let testInput = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

data Cell = Cell Bool Int deriving (Show)

cellActive :: Cell -> Bool
cellActive (Cell s v) = s

cellValue :: Cell -> Int
cellValue (Cell s v) = v
-- | parse a line of numbers
--
-- >>> runParser row () "" " 12 32 43 54"
-- Right [Cell False 12,Cell False 32,Cell False 43,Cell False 54]
-- >>> runParser row () "" "12 32 43 54  "
-- Right [Cell False 12,Cell False 32,Cell False 43,Cell False 54]
row :: Parser [Cell]
row = map (Cell False . read) <$> (whitespace *> sepEndBy1 (many1 digit) whitespace)

data Matrix = Matrix Bool [[Cell]] deriving (Show)

matrixWinner :: Matrix -> Bool
matrixWinner (Matrix s v) = s
-- | parse a list of rows separated by newline
--
-- >>> runParser matrix () "" "1 2 4\n 2   1  2  \n  3  23 23"
-- Right (Matrix False [[Cell False 1,Cell False 2,Cell False 4],[Cell False 2,Cell False 1,Cell False 2],[Cell False 3,Cell False 23,Cell False 23]])
matrix :: Parser Matrix
matrix = Matrix False <$> sepEndBy1 row eol

type ParseResult = ([Int], [Matrix])

-- | parse the entire data
--
-- >>> parse' dataParser "1,2,3\n\n 1 2 3\n  4  5 6  \n7 8 9 \n\n11 22 33\n 44 55 66\n 77 88 99"
-- Right ([1,2,3],[Matrix False [[Cell False 1,Cell False 2,Cell False 3],[Cell False 4,Cell False 5,Cell False 6],[Cell False 7,Cell False 8,Cell False 9]],Matrix False [[Cell False 11,Cell False 22,Cell False 33],[Cell False 44,Cell False 55,Cell False 66],[Cell False 77,Cell False 88,Cell False 99]]])
dataParser :: Parser ParseResult
dataParser = do
    inputs <- map read <$> sepBy (many digit) (char ',') <* newline
    newline
    matrixes <- sepEndBy matrix emptyLines
    return (inputs, matrixes)

type BingoValue = Either String Int
type BingoState = (Int, [Matrix])
-- The three values are:
-- - winning value
-- - winner indexes (first one is the last winner)
-- - matrixes
type BingoStateSafe = (Int, [Int], [Matrix])

-- | flip cell to active if value equals to i
cellMapper :: Int -> Cell -> Cell
cellMapper i (Cell s v) = if v == i then Cell True v else Cell s v

-- | apply cell mapping to all cells in the matrix
matrixMapper :: Int -> Matrix -> Matrix
matrixMapper i (Matrix s c) = Matrix s (map (map (cellMapper i)) c)

-- | apply matrix mapping to all matrixes
drawNumber :: Int -> BingoState -> BingoState
drawNumber i (val, matrixes) = (val, map (matrixMapper i) matrixes)

-- | verify if a matrix has any row or column with all states to true
--
-- >>> getVal (Matrix s v) = s
-- >>> getVal $ checkMatrix (Matrix False [[Cell False 1, Cell True 1, Cell False 1],[Cell True 1, Cell True 1, Cell True 1],[Cell False 1, Cell True 1, Cell False 1]])
-- True
-- >>> getVal $ checkMatrix (Matrix False [[Cell False 1, Cell True 1, Cell False 1],[Cell True 1, Cell True 1, Cell False 1],[Cell False 1, Cell True 1, Cell False 1]])
-- True
-- >>> getVal $ checkMatrix (Matrix False [[Cell False 1, Cell True 1, Cell False 1],[Cell True 1, Cell False 1, Cell True 1],[Cell False 1, Cell False 1, Cell False 1]])
-- False
checkMatrix :: Matrix -> Matrix
checkMatrix (Matrix ms mv) =
    let rows = any (all cellActive) mv
        cols = any (all cellActive) $ transpose mv
     in Matrix (rows || cols) mv
--    cols <- or . map (map (and . fst)) $ transpose mat
--    return $ Matrix (rows `and` cols) mat

computeMatValue :: Matrix -> Int
computeMatValue (Matrix s v) = sum $ map cellValue $ filter (not . cellActive) $ concat v

-- | check if any matrix has won
checkVictory :: Int -> BingoState -> BingoState
checkVictory i (win, mats) =
    let checkedMats = map checkMatrix mats
        winningMat = filter matrixWinner checkedMats
        loosingMat = filter (not . matrixWinner) checkedMats
        res = if null winningMat then win else i * computeMatValue (head winningMat)
     in (res, loosingMat)

-- | calculate bingo state update
--
playGame :: [Int] -> State BingoState BingoValue
playGame [] = return $ Left "Nobody won!"
playGame (i:xs) = do
    st <- get
    -- flip to true any element that matches the i element
    put $ checkVictory i . drawNumber i $ st
    (res, mats) <- get
    case res of
      0 -> playGame xs
      val -> return $ Right val

-- | calculate bingo updates
-- 
-- >>> bingoSolver $ parse' dataParser testInput
-- Right 4512
bingoSolver :: Either ParseError ParseResult -> BingoValue
bingoSolver (Left err) = Left (show err)
bingoSolver (Right pr) = evalState (playGame $ fst pr) (0, snd pr)

-- | should output the correct value give in the test input
part1 :: [String] -> String
part1 = show . bingoSolver . parse' dataParser . unlines

playGameSafe :: [Int] -> State BingoState BingoValue
playGameSafe [] = do
    (win, mats) <- get
    case win of
      0 -> return $ Left "No winner!"
      val -> return $ Right val
playGameSafe (i:xs) = do
    st <- get
    -- flip to true any element that matches the i element
    put $ checkVictory i . drawNumber i $ st
    playGameSafe xs

-- | calculate bingo updates
-- 
-- >>> bingoSolverSafe $ parse' dataParser testInput
-- Right 1924
bingoSolverSafe :: Either ParseError ParseResult -> BingoValue
bingoSolverSafe (Left err) = Left (show err)
bingoSolverSafe (Right pr) = evalState (playGameSafe $ fst pr) (0, snd pr)

-- | should output the correct value give in the test input
--
part2 :: [String] -> String
part2 = show . bingoSolverSafe . parse' dataParser . unlines

main :: IO ()
main = interact $ solution part1 part2
