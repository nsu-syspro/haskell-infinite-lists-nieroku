{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task3 where

import Data.Function
import Data.Ratio
import Task2

-- | Power series represented as infinite stream of coefficients
--
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
newtype Series a = Series
  { -- | Returns coefficients of given power series
    --
    -- For following series
    --   @a0 + a1 * x + a2 * x^2 + ...@
    -- coefficients would be
    --   @a0, a1, a2, ...@
    coefficients :: Stream a
  }
  deriving (Show)

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
x :: (Num a) => Series a
x = Series (fromList 0 [0, 1])

cons :: a -> Series a -> Series a
cons a0 = Series . Stream a0 . coefficients

uncons :: Series a -> (a, Series a)
uncons (Series (Stream a0 a')) = (a0, Series a')

instance (Num a) => Num (Series a) where
  fromInteger n = Series (fromList 0 [fromInteger n])

  negate = (*:) (-1)

  as + bs = Series ((liftA2 (+) `on` coefficients) as bs)

  a * b = cons (a0 * b0) (a0 *: b' + a' * b)
    where
      (a0, a') = uncons a
      (b0, b') = uncons b

  abs = undefined

  signum = undefined

instance (Fractional a) => Fractional (Series a) where
  fromRational r = Series (fromList 0 [fromRational r])

  a / b = cons (a0 / b0) ((a' - (a0 / b0) *: b') / b)
    where
      (a0, a') = uncons a
      (b0, b') = uncons b

-- | Multiplies power series by given number
--
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
infixl 7 *:

(*:) :: (Num a) => a -> Series a -> Series a
(*:) a = Series . fmap (* a) . coefficients

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
gen :: Series (Ratio Integer) -> Stream Integer
gen = fmap numerator . coefficients

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
ones :: Stream Integer
ones = gen (1 / (1 - x))

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
nats :: Stream Integer
nats = gen (1 / ((1 - x) * (1 - x)))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: Stream Integer
fibs = gen (x / (1 - x - x * x))
