{-# LANGUAGE DerivingStrategies #-}

module Components.Tile
  ( Tile (..),
  )
where

import Classes (Renderable (..))

data Tile = X | O | U | A
  deriving stock (Eq, Show)

instance Renderable Tile where
  render X = "⨯" -- empty tile
  render O = "⊙" -- filled tile
  render U = "⋅" -- unknown tile, potentially X or O
  render A = "⚬" -- ambiguous tile, no meaning attached, used by Range