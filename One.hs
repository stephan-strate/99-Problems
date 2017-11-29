module Problems.One where

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