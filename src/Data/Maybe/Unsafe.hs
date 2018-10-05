{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe.Unsafe
-- Copyright   :  (C) 2018 Michael J. Klein
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Use `reallyUnsafePtrEquality` to implement `Maybe`.
--
--
-- Currently, this is pretty broken:
--
-- @
--  λ> putStrLn$unlines[show(fromUMaybe<$>[unothing,pure()]),show(toUMaybe<$>[Nothing,Just()])]
--  [Just (),Just ()]
--  [UJust (),UJust ()]
-- @
--
----------------------------------------------------------------------------
module Data.Maybe.Unsafe where

import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.IORef
import Data.Semigroup
import GHC.Prim
import GHC.Types
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- | `Maybe` implemented using `reallyUnsafePtrEquality`
newtype UMaybe a = UMaybe
  { unsafeGetUMaybe :: a
  }

instance Eq a => Eq (UMaybe a) where
  x == y = fromUMaybe x == fromUMaybe y

instance Ord a => Ord (UMaybe a) where
  compare x y = compare (fromUMaybe x) (fromUMaybe y)

instance Show a => Show (UMaybe a) where
  show = ('U' :) . show . fromUMaybe

-- | @`unsafeCoerce` ()@
{-# NOINLINE unothing #-}
unothing :: UMaybe a
unothing = (unsafeCoerce# ()) -- ((),()))

-- {-# NOINLINE bottom #-}
-- bottom :: a
-- bottom = let x = x in x

-- | `maybe` for `UMaybe`
{-# NOINLINE umaybe #-}
umaybe :: b -> (a -> b) -> UMaybe a -> b
umaybe x0 f !um = case (unsafeCoerce# unothing, unsafeCoerce# um) of
                    ~(x,y) -> case reallyUnsafePtrEquality# x (y :: ()) of
                      1# -> x0
                      0# -> f $ coerce um
                      _ -> error "impossible"

-- | Convert to `UMaybe`
{-# INLINE toUMaybe #-}
toUMaybe :: Maybe a -> UMaybe a
toUMaybe Nothing = unothing
toUMaybe (Just x) = coerce x

-- | Convert from `UMaybe`
{-# INLINE fromUMaybe #-}
fromUMaybe :: UMaybe a -> Maybe a
fromUMaybe = umaybe Nothing Just

instance Semigroup a => Semigroup (UMaybe a) where
  (<>) = umaybe id $ coerce . liftM2 umaybe id (<>)

instance Semigroup a => Monoid (UMaybe a) where
  mempty = unothing

instance Foldable UMaybe where
  foldr f x = umaybe x (`f` x)
  foldMap f = umaybe mempty f

instance Functor UMaybe where
  fmap f = umaybe unothing (coerce . f)

instance Traversable UMaybe where
  traverse f = umaybe (pure unothing) (fmap coerce . f)

instance Applicative UMaybe where
  pure = coerce
  (<*>) = umaybe (const unothing) fmap

instance Alternative UMaybe where
  empty = unothing
  (<|>) = umaybe id (const . coerce)

instance Monad UMaybe where
  (>>=) = flip $ umaybe unothing

instance MonadPlus UMaybe where
  mzero = unothing
  mplus = umaybe id (const . coerce)

-- | Run tests
mainUMaybe :: IO ()
mainUMaybe = do
  mapM_ quickBatch testBatchesUMaybe

instance Model1 UMaybe UMaybe where
  model1 = id

instance Model (UMaybe a) (UMaybe a) where
  model = id

instance Arbitrary1 UMaybe where
  liftArbitrary gen = oneof [return unothing, coerce gen]
  liftShrink shr = umaybe [unothing] ((unothing :) . coerce . shr)

instance Arbitrary a => Arbitrary (UMaybe a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance EqProp Ordering where
  (=-=) = eq

instance EqProp a => EqProp (UMaybe a) where
  x =-= y =
    umaybe
      (umaybe (property True) (const $ property False) y)
      (umaybe (const $ property False) (=-=) y)
      x

-- | All tests
testBatchesUMaybe :: [TestBatch]
testBatchesUMaybe =
  [ semanticOrd (undefined :: UMaybe Int)
  -- , ordMorphism (toUMaybe :: Maybe Int -> UMaybe Int)
  -- , ordMorphism (fromUMaybe :: UMaybe Int -> Maybe Int)
  , semigroup (undefined :: UMaybe Ordering)
  , monoid (undefined :: UMaybe Ordering)
  , monoidMorphism (toUMaybe :: Maybe Ordering -> UMaybe Ordering)
  , monoidMorphism (fromUMaybe :: UMaybe Ordering -> Maybe Ordering)
  , semanticMonoid (undefined :: UMaybe Ordering)
  , functor (undefined :: UMaybe (Int, Int, Int))
  , functorMorphism toUMaybe
  , functorMorphism fromUMaybe
  , semanticFunctor (undefined :: UMaybe ())
  , semanticApplicative (undefined :: UMaybe ())
  , applicativeMorphism toUMaybe
  , applicativeMorphism fromUMaybe
  , semanticMonad (undefined :: UMaybe ())
  , monadMorphism toUMaybe
  , monadMorphism fromUMaybe
  , traversable (undefined :: UMaybe (Int, Int, Ordering))
  , monadOr (undefined :: UMaybe (Int, Int))
  , alternative (undefined :: UMaybe Int)
  ]


newline :: IO ()
newline = putStrLn ""
