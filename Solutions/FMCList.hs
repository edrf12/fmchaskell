{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (x : xs) = x

tail :: [a] -> [a]
tail [] = undefined 
tail (x : xs) = xs 

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = sum xs + x

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs 

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs <: x 

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys 
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc a [] = [a]
snoc a [x] = [x, a] 
snoc a (x : xs) = x : snoc a xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = undefined 
minimum [x] = x
minimum (x : (x' : xs))
  | x < x' = minimum (x : xs)
  | otherwise = minimum (x' : xs)

maximum :: Ord a => [a] -> a
maximum [] = undefined 
maximum [x] = x
maximum  (x : (x' : xs))
  | x > x' = maximum (x : xs)
  | otherwise = maximum (x' : xs)

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs 

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs 
drop n (x : xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs 
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs 
  | otherwise = x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = []
init [x] = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) +++ [xs]

-- subsequences

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = False
all p [x] = p x
all p (x : xs)
  | p x = all p xs
  | otherwise = False

and :: [Bool] -> Bool
and [x] = x
and (False : _) = False
and (_ : xs) = and xs

or :: [Bool] -> Bool
or [x] = x
or (True : _) = True
or (_ : xs) = or xs

concat :: [[a]] -> [a]
concat [xs] = xs
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False 
elem' x (x' : xs) = x == x' || elem' x xs 

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(x : xs) !! n = xs !! (n - 1)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

