{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O = True
    O == S n = False
    S n == S m = n == m 

instance Ord Nat where

    O <= n = True
    n <= O = False
    S n <= S m = n <= m 

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = one 
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n 

odd :: Nat -> Bool
odd n = not (even n)

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n + m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
O <-> _ = O
n <-> O = n
S n <-> S m = n <-> m 

monus :: Nat -> Nat -> Nat
monus = (<->)

infixl 6 <->

-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> O = O
n <*> S m = (n <*> m) + n

times :: Nat -> Nat -> Nat
times = (<*>)

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
n <^> S m = (n <^> m) * n

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "divide by O" 
n </> S m =
  case n <-> m of
    O -> O
    _ -> S ((n <-> S m) </> S m)

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = n <-> (n</>m) * m

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = error "divide by O" 
eucdiv (n, S m) =
  case n <-> m of
    O -> (O, n)
    nm -> (q, r)
      where
        q = S q'
        r = r'
        (q', r') = eucdiv (n <-> S m, S m)
    

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n O = n
dist O m = m
dist (S n) (S m) = dist n m

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = factorial n <*> S n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined 
lo O _ = undefined 
lo _ (S O) = O
lo b a =
  case a</>b of
    O -> O
    n -> S (lo b n)

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat x
  | x < 0 = undefined 
  | x == 0 = O
  | otherwise = S (toNat (x - 1))
-- toNat 0 = O
-- toNat n = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat n = 1 + fromNat (n <-> S O)


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger x - 1)

