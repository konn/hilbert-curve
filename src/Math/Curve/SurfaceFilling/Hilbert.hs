{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math.Curve.SurfaceFilling.Hilbert (
  hilbertCurve,
  drawHilbertCurve,
  hilbertCurvePath,
  hilbert0,
  hilbertStep,
  OrientedCurve (..),
  curveTrail,
  start,
  end,
  OpenSide (..),
  Sign (..),
  Quad (..),
  Quadrant (..),
  Rotate (..),
  Invert (..),
) where

import Control.Comonad.Cofree
import Control.Lens hiding (at, index, (:<))
import Control.Monad (join)
import Data.Distributive (Distributive (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Adjunction (unzipR)
import Data.Functor.Classes (Show1)
import Data.Functor.Rep
import Data.Generics.Labels ()
import Diagrams (Point)
import Diagrams hiding (Point (P))
import Diagrams qualified as Dia
import Diagrams.Prelude (Additive, black, white)
import GHC.Generics (Generic, Generic1, Generically1 (..))
import Generic.Data ()
import Language.Haskell.TH.Syntax (Lift)

hilbert0 :: OrientedCurve
hilbert0 = OrientedCurve {side = U, sign = P}

hilbertStep :: OrientedCurve -> Quad OrientedCurve
hilbertStep curve = case curve of
  OrientedCurve U P -> tabulate \case
    UL -> invert $ counterClockwise curve
    UR -> invert $ clockwise curve
    DL -> curve
    DR -> curve
  OrientedCurve L P -> counterClockwise $ hilbertStep $ OrientedCurve U P
  OrientedCurve R P -> clockwise $ hilbertStep $ OrientedCurve U P
  OrientedCurve D P -> clockwise $ clockwise $ hilbertStep $ OrientedCurve U P
  OrientedCurve _ N -> invert $ hilbertStep $ invert curve

hilbertCurve :: Cofree Quad OrientedCurve
hilbertCurve = coiter hilbertStep hilbert0

drawHilbertCurve ::
  ( V b ~ V2
  , N b ~ Double
  , Renderable (Path V2 Double) b
  ) =>
  Word ->
  Diagram b
drawHilbertCurve n =
  hilbertCurvePath n & center & pad 1.125 & bg white

data Endpoints a = EndpointCoords
  { start :: !a
  , end :: !a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Applicative, Monad) via Co Endpoints
  deriving anyclass (Representable, Additive)

instance Distributive Endpoints where
  distribute = distributeRep
  {-# INLINE distribute #-}

type instance V (Endpoints a) = V a

type instance N (Endpoints a) = N a

instance (Transformable a) => Transformable (Endpoints a) where
  transform t eps = fmap (Dia.transform t) eps

instance (HasOrigin a) => HasOrigin (Endpoints a) where
  moveOriginTo newOrig eps =
    EndpointCoords
      { start = moveOriginTo newOrig eps.start
      , end = moveOriginTo newOrig eps.end
      }

hilbertCurvePath ::
  forall b.
  ( V b ~ V2
  , N b ~ Double
  , Renderable (Path V2 Double) b
  ) =>
  Word -> Diagram b
hilbertCurvePath !n = fst $ loop 0 hilbertCurve
  where
    loop :: Word -> Cofree Quad OrientedCurve -> (Diagram b, Endpoints (Point V2 Double))
    loop !i (curve :< curves)
      | i == n =
          let dia = strokeCurve curve
              eps =
                EndpointCoords
                  (toRelativePoint $ start curve)
                  (toRelativePoint $ end curve)
           in (dia, eps)
      | otherwise =
          let ts = curveTrail curve
              (dia, epss) =
                unzipR $
                  imap
                    ( \qua m ->
                        loop (i + 1) m
                          & scale 0.5
                          & moveToQuadrance qua
                    )
                    curves
              ts' = map (index epss) ts

              diagram =
                mconcat
                  [ fold dia
                  , foldMap
                      ( \(src, tgt) ->
                          src ~~ tgt & lc black
                      )
                      (zip ((.end) <$> ts') $ drop 1 $ (.start) <$> ts')
                  ]
              eps' =
                join
                  EndpointCoords
                    { start = index epss (start curve)
                    , end = index epss (end curve)
                    }
           in (diagram, eps')

moveToQuadrance :: (InSpace V2 Double t, HasOrigin t) => Quadrant -> t -> t
moveToQuadrance = moveTo . toRelativePoint

strokeCurve ::
  ( V b ~ V2
  , N b ~ Double
  , Renderable (Path V2 Double) b
  ) =>
  OrientedCurve -> Diagram b
strokeCurve =
  center
    . lc black
    . stroke
    . lineFromVertices
    . map toRelativePoint
    . curveTrail

toRelativePoint :: (Fractional n) => Quadrant -> Point V2 n
toRelativePoint UL = mkP2 (-0.25) 0.25
toRelativePoint DL = mkP2 (-0.25) (-0.25)
toRelativePoint DR = mkP2 0.25 (-0.25)
toRelativePoint UR = mkP2 0.25 0.25

-- | Which side of square is open?
data OpenSide = U | D | L | R
  deriving (Show, Eq, Ord, Generic, Lift)

class Rotate a where
  clockwise :: a -> a
  counterClockwise :: a -> a

instance Rotate OpenSide where
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

data OrientedCurve = OrientedCurve
  { side :: {-# UNPACK #-} !OpenSide
  , sign :: {-# UNPACK #-} !Sign
  }
  deriving (Show, Eq, Ord, Generic)

instance Invert OrientedCurve where
  invert = #sign %~ invert

instance Rotate OrientedCurve where
  clockwise = #side %~ clockwise
  counterClockwise = #side %~ counterClockwise

data Quadrant = UL | DL | DR | UR
  deriving (Show, Eq, Ord, Generic)

instance Rotate Quadrant where
  counterClockwise UL = DL
  counterClockwise DL = DR
  counterClockwise DR = UR
  counterClockwise UR = UL

  clockwise UL = UR
  clockwise UR = DR
  clockwise DR = DL
  clockwise DL = UL

curveTrail :: OrientedCurve -> [Quadrant]
{-# INLINE curveTrail #-}
curveTrail = \curve ->
  case curve.side of
    U -> case curve.sign of
      P -> [UL, DL, DR, UR]
      N -> [UR, DR, DL, UL]
    L -> case curve.sign of
      P -> [DL, DR, UR, UL]
      N -> [UL, UR, DR, DL]
    D -> case curve.sign of
      P -> [DR, UR, UL, DL]
      N -> [DL, UL, UR, DR]
    R -> case curve.sign of
      P -> [UR, UL, DL, DR]
      N -> [DR, DL, UL, UR]

start :: OrientedCurve -> Quadrant
start OrientedCurve {..} =
  case side of
    U -> case sign of
      P -> UL
      N -> UR
    D -> case sign of
      P -> DR
      N -> DL
    L -> case sign of
      P -> DL
      N -> UL
    R -> case sign of
      P -> UR
      N -> DR

end :: OrientedCurve -> Quadrant
end = start . invert

data Quad a = Quad
  { upperLeft :: a
  , downLeft :: a
  , downRight :: a
  , upperRight :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1)
  deriving (Applicative, Show1) via Generically1 Quad

instance Monad Quad where
  (>>=) = bindRep
  {-# INLINE (>>=) #-}

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
      { upperLeft = f UL
      , downLeft = f DL
      , downRight = f DR
      , upperRight = f UR
      }
  {-# INLINE tabulate #-}
  index = flip \case
    UL -> (.upperLeft)
    DL -> (.downLeft)
    DR -> (.downRight)
    UR -> (.upperRight)
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
