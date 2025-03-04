module Exercise1.App.Data.BoundedInt
  ( BoundedInt
  , boundedInt
  , fromInt
  , toInt
  )
  where

import Prelude

import Prim.Int (class Add, class Compare)
import Prim.Ordering (LT)
import Control.Alternative (class Alternative, guard)
import Data.Foldable (intercalate)
import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

-- This module uses the smart constructor approach
-- to let us represent integer values that must
-- exist within an inclusive range. Therefore
-- it's important not to expose the constructor.
-- The `boundedInt` function is useful to avoid
-- unsafe/partial functions if we want a constant
-- that can be understood at compile time to be correct.

newtype BoundedInt :: Int -> Int -> Type
newtype BoundedInt a b = BoundedInt Int

derive newtype instance Eq (BoundedInt a b)
derive newtype instance Ord (BoundedInt a b)

boundedInt :: forall @a a' @b b' @i
  .  Reflectable i Int
  => Add (-1) a a'
  => Add 1 b b'
  => Compare a' i LT
  => Compare i b' LT
  => BoundedInt a b
boundedInt = BoundedInt (reflectType (Proxy @i))

fromInt :: forall @a @b f
  .  Alternative f
  => Reflectable a Int
  => Reflectable b Int
  => Int -> f (BoundedInt a b)
fromInt i = 
  let
    lower = reflectType (Proxy @a)
    upper = reflectType (Proxy @b)
  in
    guard (between lower upper i) $> BoundedInt i

toInt :: forall @a @b. BoundedInt a b -> Int
toInt (BoundedInt i) = i

--------------------------------------------------------------------------------

instance (Reflectable a Int, Reflectable b Int) => Show (BoundedInt a b) where
  show (BoundedInt i) =
    let
      lower = "@" <> show (reflectType (Proxy @a))
      upper = "@" <> show (reflectType (Proxy @b))
      val = "@" <> show i
    in
      intercalate " " ["boundedInt", lower, upper, val]

--------------------------------------------------------------------------------
