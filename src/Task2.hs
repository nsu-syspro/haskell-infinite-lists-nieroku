{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

import Control.Applicative
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Prelude hiding (drop, filter, repeat)

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
  show s = "[" ++ (intercalate ", " . map show . take 13 . toList $ s) ++ ", ...]"

instance Functor Stream where
  fmap = liftA

instance Applicative Stream where
  pure = repeat

  (Stream f fs) <*> (Stream a as) = Stream (f a) (fs <*> as)

instance Foldable Stream where
  foldMap f (Stream x xs) = f x <> foldMap f xs

  foldr f _ (Stream x xs) = f x (foldr f undefined xs)

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
fromList :: a -> [a] -> Stream a
fromList fill [] = Stream fill (fromList fill [])
fromList fill (x : xs) = Stream x (fromList fill xs)

drop :: Integer -> Stream a -> Stream a
drop n xxs@(Stream _ xs)
  | n <= 0 = xxs
  | otherwise = drop (pred n) xs

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Stream x xs) = (if p x then Stream x else id) (filter p xs)

repeat :: a -> Stream a
repeat = flip fromList []

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
unfold :: (b -> (a, b)) -> b -> Stream a
unfold gen b = let (a, b') = gen b in Stream a (unfold gen b')

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
nats :: Stream Integer
nats = unfold (\n -> (n, succ n)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: Stream Integer
fibs = unfold (\(a, b) -> (a, (b, a + b))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
primes :: Stream Integer
primes = unfold sieve (drop 1 nats)

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream p xs) = (p, filter (\x -> x `rem` p /= 0) xs)
