{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Ranges.RangeBlockSet
  ( RangeBlockSet (..),
    RangeBlockSetIso (..),
    RangeBlockDimension (..),
    RangeBlockMatrix (..),
    dim,
    wdt,
    hgt,
    fit,
  )
where

import Classes
  ( Lattice (..),
    Renderable (..),
    Valid (..),
    ValidLattice,
  )
import Components.Hints (HintDimension (..), HintMatrix (..), HintSet (..))
import Components.Ranges.Range (Range (..))
import Components.Ranges.RangeBlock (RangeBlock (..), RangeBlockIso)
import Components.Ranges.RangeSet (RangeSet (..))
import Data.List (nub)
import Data.Types.Isomorphic (Injective (..), Iso)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data RangeBlockSet = RBS
  { rbs :: [RangeBlock],
    rbsWdt :: Natural
  }
  deriving stock (Eq, Show, Generic)

type RangeBlockSetIso = (RangeSet, [Natural], Natural)

instance Renderable RangeBlockSet where
  render (RBS {rbs, rbsWdt}) = unlines $ map aux rbs
    where
      aux rb@(RB (R _ rgt) rbWdt) =
        concat
          [ render rb,
            replicate
              ( fromIntegral
                  (if rbsWdt >= rgt then rbsWdt - rgt else 0)
              )
              ' ',
            "| ",
            show rbWdt
          ]

instance Valid RangeBlockSet where
  valid r =
    let d :: RangeBlockSetIso
        d@(rs, _, wdt) = to r
     in valid rs && size rs <= wdt

unsafeOp ::
  (RangeSet -> RangeSet -> RangeSet) ->
  RangeBlockSet ->
  RangeBlockSet ->
  RangeBlockSet
unsafeOp op a b =
  let da :: RangeBlockSetIso
      da@(rsa, wsa, wdta) = to a
      db :: RangeBlockSetIso
      db@(rsb, wsb, wdtb) = to b
   in if wsa == wsb && wdta == wdtb
        then to (op rsa rsb, wsa, wdta)
        else
          error
            "algebraic operations on range block sets\
            \ must be of same width specifications"

instance Lattice RangeBlockSet where
  widen = unsafeOp widen
  narow = unsafeOp narow
  size rbs =
    let d :: RangeBlockSetIso
        d@(rs, _, _) = to rbs
     in size rs

instance ValidLattice RangeBlockSet

instance Injective RangeBlockSet RangeBlockSetIso where
  to (RBS rbs rbsWdt) =
    let res@(rs, ws) = unzip $ map (\rb -> (rng rb, rbWdt rb)) rbs
     in (RS rs, ws, rbsWdt)

instance Injective RangeBlockSetIso RangeBlockSet where
  to (RS rs, ws, wdt) =
    let rb = fromIntegral $ length ws - 1
     in RBS
          { rbs = zipWith (curry to) rs ws,
            rbsWdt = wdt
          }

instance Iso RangeBlockSet RangeBlockSetIso

fit :: RangeBlockSet -> RangeBlockSet
fit (RBS rbs w) = RBS (aux rbs) w
  where
    aux (x1@(RB (R l1 r1) w1) : x2@(RB (R l2 r2) w2) : xs) =
      let l2' = if l2 <= l1 + w1 then l1 + w1 + 1 else l2
       in case aux $ RB (R l2' r2) w2 : xs of
            xs@(x2'@(RB (R l2' r2') _) : _) ->
              let r1' = if r1 + w2 >= r2' then r2' - w2 - 1 else r1
               in (RB (R l1 r1') w1 : xs)
            xs -> xs
    aux xs = xs

-- HintSet to RangeBlockSet ---------------------------------------------------

instance Injective HintSet RangeBlockSet where
  to (HS [] w) = RBS [] w
  to (HS xs@(x : _) w) =
    case aux xs 0 of
      (ns, off) -> RBS (to $ node 0 (x - 1 + off) x : ns) w
    where
      node l r = RB (R l r)
      aux (x : xs@(y : _)) acc =
        let acc' = acc + x + 1
         in case aux xs acc' of
              (ns, off) ->
                let off' = if off == 0 then w - acc' - x else off
                 in (node acc' (acc' + x + off') y : ns, off')
      aux [x] acc = ([], 0)
      aux [] _ = ([], 0)

instance Injective RangeBlockSet HintSet where
  to (RBS {rbs, rbsWdt}) = HS {hints = map rbWdt rbs, hintWdt = rbsWdt}

instance Iso RangeBlockSet HintSet

newtype RangeBlockDimension = RBD [RangeBlockSet]
  deriving stock (Show, Eq, Generic)

instance Valid RangeBlockDimension where
  valid (RBD rbss) = all valid rbss && length (nub $ map rbsWdt rbss) == 1

instance Injective HintDimension RangeBlockDimension where
  to (HD hss) = RBD $ map to hss

instance Injective RangeBlockDimension HintDimension where
  to (RBD rbss) = HD $ map to rbss

instance Iso HintDimension RangeBlockDimension

dim :: RangeBlockDimension -> Natural
dim (RBD rbss) = if null rbss then 0 else fromIntegral $ length rbss

data RangeBlockMatrix = RBM RangeBlockDimension RangeBlockDimension
  deriving stock (Eq, Show, Generic)

instance Valid RangeBlockMatrix where
  valid (RBM rows cols) = valid rows && valid cols

instance Injective HintMatrix RangeBlockMatrix where
  to (HM rows cols) = RBM (to rows) (to cols)

instance Injective RangeBlockMatrix HintMatrix where
  to (RBM rows cols) = HM (to rows) (to cols)

instance Iso HintMatrix RangeBlockMatrix

wdt :: RangeBlockMatrix -> Natural
wdt (RBM rows _) = dim rows

hgt :: RangeBlockMatrix -> Natural
hgt (RBM _ cols) = dim cols