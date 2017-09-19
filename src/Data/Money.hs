{-# LANGUAGE RankNTypes #-}

module Data.Money
  (
    Money(Money)
  , money
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

-- | A representation of monetary values.
--
--   The @Semigroup@ instance allows amounts of money to be added together.
--
--   Any num instances present are hidden, as multiplying money by money,
--   for example, doesn't make any sense.
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

infixl 6 $+$
($+$) :: Num a => Money a -> Money a -> Money a
($+$) = (<>)

infixl 6 $-$
($-$) :: Num a => Money a -> Money a -> Money a
($-$) (Money m) (Money n) = Money (m - n)

infixr 7 *$
(*$) :: Num a => a -> Money a -> Money a
(*$) x (Money m) = Money (x * m)

infixl 7 $*
($*) :: Num a => Money a -> a -> Money a
($*) = flip (*$)

infixl 7 $/
($/) :: Fractional a => Money a -> a -> Money a
($/) (Money m) x = Money (m/x)

infixl 7 $/$
($/$) :: Fractional a => Money a -> Money a -> a
($/$) (Money n) (Money m) = n / m

infixr 8 $^
($^) :: (Num a, Integral b) => Money a -> b -> Money a
($^) (Money m) x = Money (m ^ x)

infixr 8 $^^
($^^) :: (Fractional a, Integral b) => Money a -> b -> Money a
($^^) (Money m) x = Money (m ^^ x)

infixr 8 $**
($**) :: Floating a => Money a -> a -> Money a
($**) (Money m) x = Money (m ** x)

