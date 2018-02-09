module Problems.Two where

-- Problem 11

-- Problem 14
dupli :: Eq a => [a] -> [a]
dupli = foldl (\xs x -> if (x `elem` xs) then xs else xs ++ [x]) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli ls n = foldl (\xs x -> xs ++ handle x) [] ls
    where
        handle x = map (\_ -> x) [1..n]

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery ls n = loop ls n n
    where
        loop (_ : xs) n 0 = loop xs n n
        loop (x : xs) n curr = x : loop xs n (curr - 1)