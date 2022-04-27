{-# LANGUAGE MultiParamTypeClasses #-}

module Classes
  ( Valid (..),
    Lattice (..),
    ValidLattice (..),
    Algebraic (..),
    Positional (..),
    Renderable (..),
  )
where

import Numeric.Natural (Natural)

class Renderable a where
  render :: a -> String
  display :: a -> IO ()
  display a = putStrLn $ render a

class Valid a where
  valid :: a -> Bool

-- | widen gives least upper bound, narrow greatest lower bound
class Lattice a where
  widen :: a -> a -> a -- generate minimum covering for both
  narow :: a -> a -> a -- generate maximum subset of both
  size :: a -> Natural

class (Valid a, Lattice a) => ValidLattice a where
  safeOp :: (a -> a -> a) -> a -> a -> Maybe a
  safeOp op a b = let r = op a b in if valid r then Just r else Nothing
  safeWiden :: a -> a -> Maybe a
  safeWiden = safeOp widen
  safeNarow :: a -> a -> Maybe a
  safeNarow = safeOp narow

-- | gives union and intersection sets of operands
class Algebraic a where
  union :: a -> a -> a
  intrs :: a -> a -> a

class Positional a where
  leftOf :: a -> Natural -> Bool
  rightOf :: a -> Natural -> Bool
  inOf :: a -> Natural -> Bool
  inOf r i = not (leftOf r i) && not (rightOf r i)
  outOf :: a -> Natural -> Bool
  outOf r i = leftOf r i || rightOf r i
  subset :: a -> a -> Bool
  overlap :: a -> a -> Bool
  overlapRight :: a -> a -> Bool
  overlapLeft :: a -> a -> Bool
  kiss :: a -> a -> Bool
  touch :: a -> a -> Bool
  touch a b = overlap a b || kiss a b