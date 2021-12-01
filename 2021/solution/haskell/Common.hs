module Common where

solution :: ([String] -> String) -> ([String] -> String) -> String -> String
solution part1 part2 content =
    let p1 = part1 $ lines content
        p2 = part2 $ lines content
     in "Part1: " ++ p1 ++ "\nPart2: " ++ p2
