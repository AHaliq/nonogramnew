module Bifurcation () where

import Classes
import Components.Ranges.Range
import Components.Ranges.RangeBlock
import Components.Ranges.RangeBlockSet
import Components.Ranges.RangeTile
import Components.Tile
import Data.List
import Data.Maybe
import Debug.Trace
import Numeric.Natural
import Utils
import Prelude

fitError :: String
fitError = "can only fit " ++ show O ++ " and " ++ show X

fitBlock :: RangeBlock -> RangeTile -> [RangeBlock]
fitBlock (RB rb@(R l1 r1) w) (RT rt@(R l2 r2) O)
  | subset rb rt =
    let l' = max l1 (if r2 + 1 > w then r2 - w + 1 else 0)
        r' = min r1 (l2 + w - 1)
        res = R l' r'
     in [RB res w | valid res]
  | otherwise = []
fitBlock o@(RB rb@(R l1 r1) w) (RT rt@(R l2 r2) X)
  -- exact
  | rb == rt = []
  -- subset
  | subset rb rt =
    let opl = if l1 < l2 && vpr then (pr :) else id
     in flip RB w <$> opl ([pl | r1 > r2 && vpl])
  -- overlap right
  | overlapRight rb rt = [RB pr w | vpr]
  -- overlap left
  | overlapLeft rb rt = [RB pl w | vpl]
  -- non overlap left
  | leftOf rb r2 = [RB rb w]
  -- non overlap right
  | otherwise = []
  where
    pr = R l1 (l2 - 1)
    pl = R (r2 + 1) r1
    vpr = valid pr
    vpl = valid pl
fitBlock _ _ = error fitError

type Fit = ([RangeBlock], Natural)

fitTiles :: RangeBlockSet -> [RangeTile] -> [RangeBlockSet]
fitTiles (RBS ors w) orts = map (\(rbs, _) -> RBS rbs w) $ fitaux 0 ors orts
  where
    -- | push right based on Fit and create single list item Fit
    -- | rb' is manual set right bound for X tiles
    -- | if push fails, returns empty list
    pushr :: Natural -> RangeBlock -> Fit -> [Fit]
    pushr rb' r (rbs, rb) = maybe [] (\r -> [(r : rbs, min rb' $ getRightBound r)]) $ pushRightSafe r rb

    -- | recursion call for fitaux to get list of Fits
    -- | map Fits with a pushr on a RangeBlock
    pushrRecurse :: [RangeBlock] -> [RangeTile] -> RangeBlock -> [Fit]
    pushrRecurse = pushrRecurseBounds 0 w

    -- | pushrRecurse with manual set of left and right bounds for X RangeTiles
    pushrRecurseBounds :: Natural -> Natural -> [RangeBlock] -> [RangeTile] -> RangeBlock -> [Fit]
    pushrRecurseBounds lb rb rs rts r = concatMap (pushr rb r) $ fitaux (max lb (getLeftBound r)) rs rts

    -- | push left on RangeBlock based on left bounds
    -- | returns empty list otherwise use function argument on it
    pushl :: RangeBlock -> Natural -> (RangeBlock -> [Fit]) -> [Fit]
    pushl r lb f = maybe [] f $ pushLeftSafe r lb

    -- | main auxilliary function producing list of Fits
    fitaux :: Natural -> [RangeBlock] -> [RangeTile] -> [Fit]
    fitaux lb (r@(RB (R gl _) _) : rs) (rt@(RT (R xl xr) t) : rts)
      | t == X =
        let rfs = fitBlock r rt in
          if null rfs 
            then pushrRecurse rs (rt:rts) r
            else firstElseSecond
              (concatMap (\r -> fitaux lb (r:rs) rts) rfs)
              (concatMap (pushrRecurseBounds (xl + 1) (subOrZero xl) rs rts) rfs)
      | t == O = 
        let rfs = fitBlock r rt in
          if null rfs || xr < gl
            then []
            else firstElseSecond
              (concatMap (\r -> fitaux lb (r:rs) rts) rfs)
              (concatMap (pushrRecurse rs rts) rfs)
      | otherwise = error fitError
    fitaux lb [] ((RT _ t) : rts)
      | t == X = fitaux lb [] rts
      | t == O = []
      | otherwise = error fitError
    fitaux lb (r : rs) [] = pushl r lb (pushrRecurse rs [])
    fitaux _ [] [] = [([], w)]
