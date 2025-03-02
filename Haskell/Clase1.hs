module Clase1 where

-- Suma de números (implementación de sum)
sumal :: [Int] -> Int
sumal [] = 0
sumal (x:xs) = x + sumal xs
-- se puede hacer con foldr
-- sumal = foldr (+) 0

-- Concatenación de strings
sumac :: [[Char]] -> [Char]
sumac [] = ""
sumac (x:xs) = x ++ sumac xs
-- sumac = foldr (++) ""

-- Gets the element at the given index    
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:_) 0 = x
elementAt (_:xs) n
    | n < 0 = error "Index out of bounds"
    | otherwise = elementAt xs (n-1)

-- Gets the last element of a list
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Gets the last but one element of a list
myLastByOne :: [a] -> a
myLastByOne [] = error "Empty list"
myLastByOne [x] = error "List with only one element"
myLastByOne [x,_] = x
myLastByOne (_:xs) = myLastByOne xs

-- Gets the length of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Reverses a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Checks if a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = xs == myReverse xs


compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress [x, y]
    |x == y = [x]
    |otherwise = [x, y]
compress (x: y: xs)
    |x == y = compress (x: xs)
    |otherwise = x : compress (y: xs)

-- Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' [x] xs
  where
    pack' current [] = [current]
    pack' current (y:ys)
      | y == head current = pack' (y:current) ys
      | otherwise         = current : pack' [y] ys


n :: Int
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]