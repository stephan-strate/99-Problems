module Problems.One where
import Data.List

-- Problem 1
myLast :: [a] -> a
myLast []           = error "No elements in list."
myLast (x : [])     = x
myLast (_ : xs)     = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast []            = error "No elements in list."
myButLast (_ : [])      = error "Not enough elements in list."
myButLast (x : _ : [])  = x
myButLast (_ : xs)      = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x : _) 1             = x
elementAt _ n | n <= 0          = error "Negative index."
elementAt (_ : xs) n | n > 0    = elementAt xs (n - 1)

-- Problem 4
myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\x xs -> xs : x) []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome [_]    = True
isPalindrome xs     = (isPalindrome (init (tail xs))) && ((head xs) == (myLast xs))

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a)        = [a]
flatten (List [])       = []
flatten (List (x:xs))   = flatten x ++ flatten (List xs)

-- Problem 8
compress :: [Char] -> [Char]
compress input = foldl (\xs elem -> if (not (isElemInList xs elem)) then xs ++ [elem] else xs) [] input
    where
        isElemInList :: (Eq a) => [a] -> a -> Bool
        isElemInList arr val = foldl (||) False (map (== val) arr)

-- Problem 9
pack :: [Char] -> [[Char]]
pack = group

-- Problem 10
encode :: [Char] -> [(Int, Char)]
encode input = map (build) (group input)
    where
        build :: [Char] -> (Int, Char)
        build ls = (length ls, head ls)