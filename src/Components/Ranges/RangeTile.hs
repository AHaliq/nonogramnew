{-# LANGUAGE DerivingStrategies #-}

module Components.Ranges.RangeTile
  ( RangeTile (..),
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
import Components.Tile (Tile)

data RangeTile = RT
  { trng :: Range,
    tile :: Tile
  }
  deriving stock (Eq, Show)

instance Renderable RangeTile where
  render (RT w@(R l _) t) = replicate (fromIntegral l) ' ' ++ concat (replicate (fromIntegral $ size w) (render t))

instance Valid RangeTile where
  valid (RT r _) = valid r

latticeError :: String
latticeError = "lattice operations on RangeTile must be of same tile"

instance Lattice RangeTile where
  widen (RT r1 w1) (RT r2 w2)
    | w1 == w2 = RT (widen r1 r2) w1
    | otherwise = error latticeError
  narow (RT r1 w1) (RT r2 w2)
    | w1 == w2 = RT (narow r1 r2) w1
    | otherwise = error latticeError
  size (RT r _) = size r

instance ValidLattice RangeTile

instance Positional RangeTile where
  leftOf (RT r _) = leftOf r
  rightOf (RT r _) = rightOf r
  subset (RT r1 _) (RT r2 _) = subset r1 r2
  overlap (RT r1 _) (RT r2 _) = overlap r1 r2
  kiss (RT r1 _) (RT r2 _) = kiss r1 r2
  overlapRight (RT r1 _) (RT r2 _) = overlapRight r1 r2
  overlapLeft (RT r1 _) (RT r2 _) = overlapLeft r1 r2
