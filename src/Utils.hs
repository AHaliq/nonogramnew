module Utils
  ( mapTuple,
    filterValid,
    subOrZero,
    firstElseSecond,
  )
where

import Control.Arrow ((***))
import Control.Monad (join, liftM2)
import Data.Maybe (isJust)
import Numeric.Natural (Natural)

mapTuple :: (b' -> c') -> (b', b') -> (c', c')
mapTuple = join (***)

filterValid :: [Maybe a1] -> Maybe [a1]
filterValid xs = foldr (liftM2 (:)) (Just []) $ filter isJust xs

subOrZero :: Natural -> Natural
subOrZero n = if n > 0 then n - 1 else 0

firstElseSecond :: [a] -> [a] -> [a]
firstElseSecond [] b = b
firstElseSecond a _ = a