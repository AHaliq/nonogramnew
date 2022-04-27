{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.Ranges.RangeBlock
  ( RangeBlock (..),
    RangeBlockIso (..),
    getLeftBound,
    getRightBound,
    pushLeftSafe,
    pushRightSafe,
  )
where

import Classes
  ( Lattice (..),
    Positional (..),
    Renderable (..),
    Valid (..),
    ValidLattice,
  )
import Components.Ranges.Range (Range (..))
import Components.Tile (Tile (..))
import Data.Types.Injective (Injective (to))
import Data.Types.Isomorphic (Iso)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data RangeBlock = RB
  { rng :: Range,
    rbWdt :: Natural
  }
  deriving stock (Eq, Show, Generic)

opRange :: (Range -> Range) -> RangeBlock -> RangeBlock
opRange op (RB {rng, rbWdt}) = RB {rng = op rng, rbWdt}

instance Renderable RangeBlock where
  render (RB rng@(R l r) rbWdt)
    | not (valid rng) = concat [show (l, r), "!<", show rbw]
    | s < rbw = concat [show (l, r), "<", show rbw]
    | otherwise =
      concat
        [ lgap,
          shadeStr,
          concat $ replicate gap $ render c,
          shadeStr
        ]
    where
      s = size rng
      rbw = fromIntegral rbWdt
      g = fromIntegral s - 2 * fromIntegral rbw
      shade = min (fromIntegral rbWdt) $ (fromIntegral s + g) `div` 2
      gap = abs g
      shadeStr = concat $ replicate shade (render U)
      c = if g < 0 then O else U
      lgap = replicate (fromIntegral l) ' '

instance Valid RangeBlock where
  valid rb = valid (rng rb) && rbWdt rb <= size (rng rb)

instance Lattice RangeBlock where
  widen (RB a x) (RB b y)
    | x == y = RB (a `widen` b) x
    | otherwise = error "cannot widen / sum range blocks of different wdt"
  narow (RB a x) (RB b y)
    | x == y = RB (a `narow` b) x
    | otherwise = error "cannot intersect / prod range blocks of different wdt"
  size = size . rng

instance ValidLattice RangeBlock

instance Positional RangeBlock where
  leftOf (RB r _) = leftOf r
  rightOf (RB r _) = rightOf r
  subset (RB r1 _) (RB r2 _) = r1 `subset` r2
  overlap (RB r1 _) (RB r2 _) = r1 `overlap` r2
  overlapRight (RB r1 _) (RB r2 _) = r1 `overlapRight` r2
  overlapLeft (RB r1 _) (RB r2 _) = r1 `overlapLeft` r2
  kiss (RB r1 _) (RB r2 _) = r1 `kiss` r2

type RangeBlockIso = (Range, Natural)

instance Injective RangeBlockIso RangeBlock where
  to (r, n) = RB r n

instance Injective RangeBlock RangeBlockIso where
  to (RB r n) = (r, n)

instance Iso RangeBlock RangeBlockIso

getLeftBound :: RangeBlock -> Natural
getLeftBound (RB (R l _) w) = l + w + 1

getRightBound :: RangeBlock -> Natural
getRightBound (RB (R _ r) w) = if r > w then r - w - 1 else 0

pushLeft :: RangeBlock -> Natural -> RangeBlock
pushLeft ori@(RB (R l r) w) lb = if lb > l then RB (R lb r) w else ori

pushRight :: RangeBlock -> Natural -> RangeBlock
pushRight ori@(RB (R l r) w) rb = if rb < r then RB (R l rb) w else ori

pushOpSafe :: (RangeBlock -> Natural -> RangeBlock) -> RangeBlock -> Natural -> Maybe RangeBlock
pushOpSafe op rb b = let res = op rb b in if valid res then Just res else Nothing

pushLeftSafe :: RangeBlock -> Natural -> Maybe RangeBlock
pushLeftSafe = pushOpSafe pushLeft

pushRightSafe :: RangeBlock -> Natural -> Maybe RangeBlock
pushRightSafe = pushOpSafe pushRight