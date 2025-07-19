{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math.Curve.SurfaceFilling.Hilbert (
  hilbertCurve,
  hilbert0,
  hilbertStep,
  OrientedCurve (..),
  trail,
  start,
  end,
  Direction (..),
  Sign (..),
  Quad (..),
  Quadrant (..),
  Rotate (..),
  Invert (..),
) where

import Control.Comonad.Cofree
import Control.Lens hiding (index)
import Data.Distributive (Distributive (..))
import Data.Functor.Classes (Show1)
import Data.Functor.Rep
import Data.Generics.Labels ()
import GHC.Generics (Generic, Generic1, Generically1 (..))
import Generic.Data ()
import Language.Haskell.TH.Syntax (Lift)

hilbert0 :: OrientedCurve
hilbert0 = OrientedCurve {direction = U, sign = P}

hilbertStep :: (Rotate a, Invert a) => a -> Quad a
hilbertStep curve = tabulate \case
  UpperLeft -> invert $ counterClockwise curve
  UpperRight -> invert $ clockwise curve
  DownLeft -> curve
  DownRight -> curve

hilbertCurve :: Cofree Quad OrientedCurve
hilbertCurve = coiter hilbertStep hilbert0

-- | Which side of square is open?
data Direction = U | D | L | R
  deriving (Show, Eq, Ord, Generic, Lift)

class Rotate a where
  clockwise :: a -> a
  counterClockwise :: a -> a

instance Rotate Direction where
  clockwise U = R
  clockwise R = D
  clockwise D = L
  clockwise L = U

  counterClockwise U = L
  counterClockwise L = D
  counterClockwise D = R
  counterClockwise R = U

{- | The _sign_ of the U curve;
The curve has sign 'P' if it starts from upper-left corner when aligned to 'U' direction.
-}
data Sign = N | P
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Lift)

class Invert a where
  invert :: a -> a

instance Invert Sign where
  invert N = P
  invert P = N

data OrientedCurve = OrientedCurve {direction :: Direction, sign :: Sign}
  deriving (Show, Eq, Ord, Generic)

instance Invert OrientedCurve where
  invert = #sign %~ invert

instance Rotate OrientedCurve where
  clockwise = #direction %~ clockwise
  counterClockwise = #direction %~ counterClockwise

data Quadrant = UpperLeft | DownLeft | DownRight | UpperRight
  deriving (Show, Eq, Ord, Generic)

instance Rotate Quadrant where
  counterClockwise UpperLeft = DownLeft
  counterClockwise DownLeft = DownRight
  counterClockwise DownRight = UpperRight
  counterClockwise UpperRight = UpperLeft

  clockwise UpperLeft = UpperRight
  clockwise UpperRight = DownRight
  clockwise DownRight = DownLeft
  clockwise DownLeft = UpperLeft

trail :: OrientedCurve -> [Quadrant]
{-# INLINE trail #-}
trail = \curve ->
  case curve.direction of
    U -> case curve.sign of
      P -> [UpperLeft, DownLeft, DownRight, UpperRight]
      N -> [UpperRight, DownRight, DownLeft, UpperLeft]
    L -> case curve.sign of
      P -> [DownLeft, DownRight, UpperRight, UpperLeft]
      N -> [UpperLeft, UpperRight, DownRight, DownLeft]
    D -> case curve.sign of
      P -> [DownRight, UpperRight, UpperLeft, DownLeft]
      N -> [DownLeft, UpperLeft, UpperRight, DownRight]
    R -> case curve.sign of
      P -> [UpperRight, UpperLeft, DownLeft, DownRight]
      N -> [DownRight, DownLeft, UpperLeft, UpperRight]

start :: OrientedCurve -> Quadrant
start OrientedCurve {..} =
  case direction of
    U -> case sign of
      P -> UpperLeft
      N -> UpperRight
    D -> case sign of
      P -> DownRight
      N -> DownLeft
    L -> case sign of
      P -> DownLeft
      N -> UpperLeft
    R -> case sign of
      P -> UpperRight
      N -> DownRight

end :: OrientedCurve -> Quadrant
end = start . invert

data Quad a = Quad
  { upperLeft :: a
  , downLeft :: a
  , downRight :: a
  , upperRight :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Show1) via Generically1 Quad

instance (Rotate a) => Rotate (Quad a) where
  clockwise = fmap clockwise . localRep counterClockwise
  {-# INLINE clockwise #-}
  counterClockwise = fmap counterClockwise . localRep clockwise
  {-# INLINE counterClockwise #-}

instance (Invert a) => Invert (Quad a) where
  invert = fmap invert
  {-# INLINE invert #-}

instance Distributive Quad where
  distribute = distributeRep
  {-# INLINE distribute #-}

instance Representable Quad where
  type Rep Quad = Quadrant
  tabulate f =
    Quad
      { upperLeft = f UpperLeft
      , downLeft = f DownLeft
      , downRight = f DownRight
      , upperRight = f UpperRight
      }
  {-# INLINE tabulate #-}
  index = flip \case
    UpperLeft -> (.upperLeft)
    DownLeft -> (.downLeft)
    DownRight -> (.downRight)
    UpperRight -> (.upperRight)
  {-# INLINE index #-}

instance FunctorWithIndex Quadrant Quad where
  imap = imapRep
  {-# INLINE imap #-}

instance FoldableWithIndex Quadrant Quad where
  ifoldMap = ifoldMapRep
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Quadrant Quad where
  itraverse = itraverseRep
  {-# INLINE itraverse #-}
