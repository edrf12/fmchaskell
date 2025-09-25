module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O  = one
isZero (S _) = zero 

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = zero 
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = one 
even (S O) = O
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = O
odd (S O) = one 
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

infixl 6 -*

-- multiplication
mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = (n * m) + n 

(*) :: Nat -> Nat -> Nat
(*) = mult

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) _ O = one 
(^) n (S m) = (n ^ m) * n 

infixr 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
(/) _ O = undefined
(/) n (S m) = 
  case n -* m of
    O -> O -- This fits cases like n / (n + m) with m != 0
    _ -> S ((n -* S m) / S m)
    
infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
(%) n m = n -* (n/m) * m

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) n m =
  case m % n of
    O -> S O
    _ -> O

infix 1 |||

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
-- absDiff n m =
  -- case n -* m of
    -- O -> m -* n 
    -- n' -> n'
absDiff n O = n
absDiff O m = m
absDiff (S n) (S m) = absDiff n m

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

