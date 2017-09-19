{-# LANGUAGE RankNTypes #-}

-- | A data type for monetary values, with associated operations and
--   only sensible instances.
module Data.Money
  (
    Money(Money)
  -- * Optics
  , money
  -- * Operators
  , ($+$)
  , ($-$)
  , (*$)
  , ($*)
  , ($/)
  , ($/$)
  , ($^)
  , ($^^)
  , ($**)
  ) where

import Data.Functor (Functor (fmap))
import Data.Foldable (Foldable (foldMap))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Profunctor (Profunctor, dimap)
import Data.Semigroup (Semigroup, (<>))
import Data.Traversable (Traversable (traverse))

-- | A newtype for monetary values represented as type @num@.
--
--   The 'Semigroup' instance allows amounts of money to be added together.
--
--   Any 'Num' instances present are hidden, as operations like multiplying
--   money by money don't make any sense.
newtype Money num =
  Money num
  deriving (Eq, Ord)

instance Show num => Show (Money num) where
  show (Money m) = '$': show m

instance Num a => Semigroup (Money a) where
  Money m <> Money n = Money (m + n)

instance Num a => Monoid (Money a) where
  mappend = (<>)
  mempty = Money 0

instance Functor Money where
  fmap f (Money n) = Money (f n)

instance Foldable Money where
  foldMap f (Money n) = f n

instance Traversable Money where
  traverse f (Money n) = fmap Money (f n)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | The raw numeric value inside monetary value
money :: Iso (Money a) (Money b) a b
money = dimap (\(Money a) -> a) (fmap Money)

-- | Add money to money. A synonym for @<>@.
infixl 6 $+$
($+$) :: Num a => Money a -> Money a -> Money a
($+$) = (<>)

-- | Subtract money from money
infixl 6 $-$
($-$) :: Num a => Money a -> Money a -> Money a
($-$) (Money m) (Money n) = Money (m - n)

-- | Multiply a scalar by money
infixr 7 *$
(*$) :: Num a => a -> Money a -> Money a
(*$) x (Money m) = Money (x * m)

-- | Multiply money by a scalar
infixl 7 $*
($*) :: Num a => Money a -> a -> Money a
($*) = flip (*$)

-- | Divide money by a scalar
infixl 7 $/
($/) :: Fractional a => Money a -> a -> Money a
($/) (Money m) x = Money (m/x)

-- | Divide money by money
infixl 7 $/$
($/$) :: Fractional a => Money a -> Money a -> a
($/$) (Money n) (Money m) = n / m

-- | Raise money to a non-negative integral power
infixr 8 $^
($^) :: (Num a, Integral b) => Money a -> b -> Money a
($^) (Money m) x = Money (m ^ x)

-- | Raise money to an integral power
infixr 8 $^^
($^^) :: (Fractional a, Integral b) => Money a -> b -> Money a
($^^) (Money m) x = Money (m ^^ x)

-- | Raise money to a floating-point power
infixr 8 $**
($**) :: Floating a => Money a -> a -> Money a
($**) (Money m) x = Money (m ** x)

