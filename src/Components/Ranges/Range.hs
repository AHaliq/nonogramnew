{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Components.Ranges.Range
  ( Range (..),
  )
where

import Classes
  ( Lattice (..),
    Positional (..),
    Renderable (..),
    Valid (..),
    ValidLattice,
  )
import Components.Tile (Tile (..))
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data Range = R
  { lft :: Natural,
    rgt :: Natural
  }
  deriving stock (Eq, Show, Generic)

instance Renderable Range where
  render r =
    replicate (fromIntegral $ lft r) ' '
      ++ concat (replicate (fromIntegral $ size r) $ render A)

instance Valid Range where
  valid R {lft, rgt} = lft <= rgt

instance Lattice Range where
  widen (R al ar) (R bl br) = R (min al bl) (max ar br)
  narow (R al ar) (R bl br) = (R (max al bl) (min ar br))
  size r = rgt r - lft r + 1

instance ValidLattice Range

instance Positional Range where
  leftOf (R l _) i = i < l
  rightOf (R _ r) i = i > r
  subset a@(R l1 r1) b@(R l2 r2) = l2 >= l1 && r2 <= r1
  overlap a@(R al ar) b@(R bl br) =
    inOf a bl || inOf a br || inOf b al || inOf b ar
  overlapRight (R l1 r1) (R l2 r2) = l2 >= l1 && l2 <= r1 && r2 > r1
  overlapLeft (R l1 r1) (R l2 r2) = r2 >= l1 && r2 <= r1 && l2 < l1
  kiss a@(R al ar) b@(R bl br) = ar + 1 == bl || br + 1 == al