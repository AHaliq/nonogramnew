{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Components.Ranges.RangeSet
  ( RangeSet (..),
    unsafeOp,
  )
where

import Classes
  ( Algebraic (..),
    Lattice (..),
    Positional (..),
    Renderable (..),
    Valid (..),
    ValidLattice,
  )
import Components.Ranges.Range (Range (..))
import Numeric.Natural (Natural)

newtype RangeSet = RS [Range]
  deriving stock (Eq, Show)

instance Renderable RangeSet where
  render (RS rs) = unlines $ map render rs

instance Valid RangeSet where
  valid (RS (y@(R _ a) : x@(R b _) : xs)) = valid y && a < b - 1 && valid (RS (x : xs))
  valid (RS xs) = all valid xs

unsafeOp :: (Range -> Range -> Range) -> RangeSet -> RangeSet -> RangeSet
unsafeOp op (RS as) (RS bs) = RS $ aux op as bs
  where
    aux op (a : as) (b : bs) = (a `op` b) : aux op as bs
    aux op [] [] = []
    aux _ _ _ = error "algebraic operations on range sets must be of same bounds"

instance Lattice RangeSet where
  widen = unsafeOp widen
  narow = unsafeOp narow
  size (RS rs) = sum $ map size rs

instance ValidLattice RangeSet

instance Algebraic RangeSet where
  union (RS xs) (RS ys) =
    let aux (x : xs) (y : ys)
          | touch x y =
            let cur = widen x y
                rest = aux xs ys
             in if null rest
                  then [cur]
                  else
                    let (r : rs) = rest
                     in if touch cur r
                          then widen cur r : rs
                          else cur : r : rs
          | leftOf x (rgt y) = y : x : aux xs ys
          | otherwise = x : aux xs (y : ys)
        aux [] ys = ys
        aux xs [] = xs
        rs = aux xs ys
     in RS rs
  intrs _ _ = error "not implemented"
