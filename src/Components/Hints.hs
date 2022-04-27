{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Components.Hints
  ( Hint,
    HintSet (..),
    HintSetIso (..),
    HintDimension (..),
    HintDimensionIso (..),
    HintMatrix (..),
    hintDimWdt,
  )
where

import Classes (Valid (..))
import Data.List (nub)
import Data.Types.Isomorphic (Injective (..), Iso)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Utils (mapTuple)

type Hint = Natural

-- Hint Set -------------------------------------------------------------------

data HintSet = HS
  { hints :: [Natural],
    hintWdt :: Natural
  }
  deriving stock (Show, Eq, Generic)

instance Valid HintSet where
  valid (HS hs wdt) =
    fromIntegral (sum hs) + length hs - 1 < fromIntegral wdt

type HintSetIso = ([Natural], Natural)

instance Injective HintSetIso HintSet where
  to (hs, wdt) = HS hs wdt

instance Injective HintSet HintSetIso where
  to (HS hints hintWdt) = (hints, hintWdt)

instance Iso HintSet HintSetIso

-- Hint Dimension -------------------------------------------------------------

newtype HintDimension = HD [HintSet]
  deriving stock (Show, Eq, Generic)

instance Valid HintDimension where
  valid (HD hss) = all valid hss && length (nub $ map hintWdt hss) == 1

type HintDimensionIso = ([[Natural]], Natural)

instance Injective HintDimensionIso HintDimension where
  to (hints, wdt) = HD $ map (\hs -> to (hs, wdt)) hints

instance Injective HintDimension HintDimensionIso where
  to (HD hss) =
    let res = map to hss
     in (map fst res, if null res then 0 else snd (head res))

instance Iso HintDimension HintDimensionIso

hintDimWdt :: HintDimension -> Natural
hintDimWdt (HD ((HS {hintWdt}) : _)) = hintWdt
hintDimWdt _ = 0

-- Hint Matrix ----------------------------------------------------------------

data HintMatrix = HM HintDimension HintDimension
  deriving stock (Show, Eq, Generic)

instance Valid HintMatrix where
  valid (HM rows columns) = valid rows && valid columns

type HintMatrixIso = ([[Natural]], [[Natural]])

instance Injective HintMatrixIso HintMatrix where
  to (rows, columns) =
    let r :: (Natural, Natural)
        r@(wdt, hgt) = mapTuple (fromIntegral . length) (columns, rows)
     in HM (to (rows, wdt)) (to (columns, hgt))

instance Injective HintMatrix HintMatrixIso where
  to (HM rows cols) =
    let rres :: (HintDimensionIso, HintDimensionIso)
        rres = (to rows, to cols)
     in mapTuple fst rres

instance Iso HintMatrix HintMatrixIso