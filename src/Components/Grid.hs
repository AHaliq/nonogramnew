{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.Grid
  ( Grid (..),
  )
where

import Classes (Renderable (..), Valid (..))
import Components.Hints (HintMatrix)
import Components.Ranges.RangeBlockSet (RangeBlockMatrix, RangeBlockSet)
import Components.Tile (Tile (..))
import Data.Map (Map, lookup, toList)
import Data.Types.Isomorphic (Injective (..), Iso)
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

newtype Grid = Grid (Map (Int, Int) Tile)
  deriving stock (Eq, Show)

instance Renderable Grid where
  render g@(Grid m) =
    let (wdt, hgt) =
          foldl (\(mx, my) ((x, y), _) -> (max x mx, max my y)) (0, 0) $
            toList m
     in unlines $ map (drawGridRow g wdt) [0 .. hgt]
    where
      drawGridRow :: Grid -> Int -> Int -> String
      drawGridRow (Grid m) wdt y =
        concatMap
          ( \x ->
              maybe (render U) render $
                lookup (x, y) m
          )
          [0 .. wdt]

data Board = Board
  { grid :: Grid,
    rbm :: RangeBlockMatrix
  }
  deriving (Eq, Show)

instance Renderable Board where
  render _ = error "to be implented"

instance Valid Board where
  valid (Board {rbm}) = valid rbm

instance Injective Board HintMatrix where
  to _ = error "to be implenented"

instance Injective HintMatrix Board where
  to _ = error "to be implemented"

instance Iso HintMatrix Board

-- getXrangesFromCol
-- getXrangesFromRow
-- getOrangesFromCol
-- getOrangesFromRow

-- TODO construct RangeBlockSetTree from X range and O range and RangeBlockSet

-- work on state data
-- you actually dont need a grid
-- you just need range of X and range of O and rangeBlock per row and column
-- then when you constraint to new tiles you store it as an action (tile and index)
-- then when switch dimension each row / col will take in actions affecting it to update its rangeof X and range of O

-- work on bifurcate RangeBlockSetTree
-- then run narrow on list of bifurcations
-- narrow should follow with push normalize
-- widen doesnt and cant push normalize as we would need to know who initiates but
-- using list iteration its hard unless its a tree and not a list

-- TODO
-- 1. push normalize                      DONE
-- 2. bifurcate RBS to RBSTree or [RBS]
-- each row has [RBS] not just RBS
-- when bifurcate leads to invalid we throw them away
-- 3. State data type