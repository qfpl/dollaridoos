{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Money where

import Control.Lens (Lens, Getter, lens, Iso, iso, over, view, to)
import Data.Monoid (Monoid, Sum(Sum, getSum), (<>))
import Data.Scientific (Scientific)

-- | A representation of monetary values.
--   The monoid instance allows amounts of money to be added together.
--   Any num instances present are hidden, as dividing money by money,
--   for example, doesn't make any sense.
newtype Money num = Money { getMoneySum :: (Sum num) }
                    deriving (Monoid, Eq, Ord)

instance Show num => Show (Money num) where
  show m = '$': (show $ view getMoney m)

-- | Scientific numbers seem an appropriate default choice to represent
--   monetary values.
type MoneyS = Money Scientific

-- | 
moneySum :: Iso (Money a) (Money b) (Sum a) (Sum b)
moneySum = iso getMoneySum Money

-- | The raw numeric value inside monetary value
getMoney :: Lens (Money a) (Money b) a b
getMoney = lens (getSum . getMoneySum) setMon
           where setMon (Money (Sum _)) z = (Money (Sum z))

makeMoney_ :: a -> Money a
makeMoney_ = Money . Sum

-- | Wrap a value into a monetary context
makeMoney :: Getter a (Money a)
makeMoney = to makeMoney_

infixl 6 $+$
($+$) :: Num a => Money a -> Money a -> Money a
($+$) = (<>)

infixr 7 *$
(*$) :: Num a => a -> Money a -> Money a
(*$) = over getMoney . (*)

infixl 7 $*
($*) :: Num a => Money a -> a -> Money a
($*) = flip (*$)

infixl 7 $/
($/) :: Fractional a => Money a -> a -> Money a
($/) m x = over getMoney (/x) m

infixr 8 $^
($^) :: (Num a, Integral b) => Money a -> b -> Money a
($^) m x = over getMoney (^x) m

infixr 8 $^^
($^^) :: (Fractional a, Integral b) => Money a -> b -> Money a
($^^) m x = over getMoney (^^x) m

infixr 8 $**
($**) :: Floating a => Money a -> a -> Money a
($**) m x = over getMoney (**x) m

