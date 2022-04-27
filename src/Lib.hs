{-# LANGUAGE DerivingStrategies #-}

module Lib
  ( someFunc,
  )
where

import Prelude

-- type RangeSetTree
-- each confirmed block contained in a baseRange will generate a range
-- each bifurcation will proceed to next confirmed block to bifurcate further
-- total ranges csp is confirmed

-- hints are in range sets in range blocks
-- RangeSetTree has [RangeSet]
-- csp of [RangeSet] is confirmed RangeSet
-- gridRow, gridColumn from Grid to range; get confirmed block widths as [Range]

-- each evaluation step of a row / column as a single function call
-- repeated function call till full evaluation rather than single one call to finish

someFunc :: IO ()
someFunc = putStrLn "someFunc"
