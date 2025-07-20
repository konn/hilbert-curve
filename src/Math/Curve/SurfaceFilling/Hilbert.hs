{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math.Curve.SurfaceFilling.Hilbert (
  hilbertCurve,
  surfaceFillingCurvePath,
  hilbert0,
  hilbertStep,
  mooreStep,
  mooreCurve,
  SurfaceFillingCurve,
  drawSurfaceFillingCurve,
  OrientedCurve (OrientedCurve, sign, side, PosCurve, NegCurve),
  curveTrail,
  start,
  end,
  OpenSide (U, L, D, R),
  Sign (..),
  Quad (..),
  Quadrant (..),
  Rotate (..),
  Invert (..),
) where

import Control.Comonad.Cofree
import Control.Lens hiding (at, index, (:<))
import Control.Lens.Internal.CTypes (Word8)
import Control.Monad (join)
import Data.Bits (Bits (..))
import Data.Coerce (coerce)
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
import GHC.Records (HasField (..))
import Generic.Data ()
import Language.Haskell.TH.Syntax (Lift)

type SurfaceFillingCurve = Cofree Quad OrientedCurve

hilbert0 :: OrientedCurve
hilbert0 = OrientedCurve {side = U, sign = P}

hilbertStep :: OrientedCurve -> Quad OrientedCurve
hilbertStep curve = case curve of
  NegCurve -> invert $ hilbertStep $ invert curve
  PosCurve -> case curve.side of
    U -> tabulate \case
      UL -> invert $ counterClockwise curve
      UR -> invert $ clockwise curve
      DL -> curve
      DR -> curve
    L -> counterClockwise $ hilbertStep $ OrientedCurve U P
    R -> clockwise $ hilbertStep $ OrientedCurve U P
    D -> clockwise $ clockwise $ hilbertStep $ OrientedCurve U P

hilbertCurve :: SurfaceFillingCurve
hilbertCurve = coiter hilbertStep hilbert0

mooreStep :: OrientedCurve -> Quad OrientedCurve
mooreStep curve = case curve of
  NegCurve -> invert $ hilbertStep $ invert curve
  PosCurve -> case curve.side of
    U -> tabulate \case
      UL -> clockwise curve
      UR -> counterClockwise curve
      DL -> clockwise curve
      DR -> counterClockwise curve
    L -> counterClockwise $ hilbertStep $ OrientedCurve U P
    R -> clockwise $ hilbertStep $ OrientedCurve U P
    D -> clockwise $ clockwise $ hilbertStep $ OrientedCurve U P

mooreCurve :: SurfaceFillingCurve
mooreCurve = coiter mooreStep hilbert0

drawSurfaceFillingCurve ::
  ( V b ~ V2
  , N b ~ Double
  , Renderable (Path V2 Double) b
  ) =>
  Word ->
  SurfaceFillingCurve ->
  Diagram b
drawSurfaceFillingCurve n curve =
  surfaceFillingCurvePath n curve & center & pad 1.125 & bg white

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

surfaceFillingCurvePath ::
  forall b.
  ( V b ~ V2
  , N b ~ Double
  , Renderable (Path V2 Double) b
  ) =>
  Word ->
  SurfaceFillingCurve ->
  Diagram b
surfaceFillingCurvePath = fmap fst . loop
  where
    loop :: Word -> Cofree Quad OrientedCurve -> (Diagram b, Endpoints (Point V2 Double))
    loop 0 (!curve :< _) =
      let dia = strokeCurve curve
          eps =
            EndpointCoords
              (toRelativePoint $ start curve)
              (toRelativePoint $ end curve)
       in (dia, eps)
    loop !n (curve :< curves) =
      let ts = curveTrail curve
          (dia, epss) =
            unzipR $
              imap
                ( \qua m ->
                    loop (n - 1) m
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
newtype OpenSide = OpenSide Word8
  deriving (Eq, Ord, Generic, Lift)

instance Show OpenSide where
  show = \case
    U -> "U"
    L -> "L"
    D -> "D"
    R -> "R"

instance Bounded OpenSide where
  minBound = U
  maxBound = R

instance Enum OpenSide where
  fromEnum (OpenSide w) = fromIntegral w
  toEnum = OpenSide . fromIntegral

pattern U :: OpenSide
pattern U = OpenSide 0b00

pattern L :: OpenSide
pattern L = OpenSide 0b01

pattern D :: OpenSide
pattern D = OpenSide 0b10

pattern R :: OpenSide
pattern R = OpenSide 0b11

{-# COMPLETE U, L, D, R #-}

instance Rotate OpenSide where
  counterClockwise (OpenSide w) =
    OpenSide $ (w + 0b01) .&. 0b11

  clockwise (OpenSide w) =
    OpenSide $ (w + 0b11) .&. 0b11

class Rotate a where
  clockwise :: a -> a
  counterClockwise :: a -> a

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

newtype OrientedCurve = OrientedCurve' Word8
  deriving (Eq, Ord, Generic)

instance Show OrientedCurve where
  show (OrientedCurve' w) =
    "OrientedCurve {side = "
      <> show (side' $ OrientedCurve' w)
      <> ", sign = "
      <> show (sign' $ OrientedCurve' w)
      <> "}"

pattern OrientedCurve :: OpenSide -> Sign -> OrientedCurve
pattern OrientedCurve {side, sign} <- (viewOC -> OrientedCurveView {sign, side})
  where
    OrientedCurve side sign = OrientedCurve' (fromIntegral $ (fromEnum sign `shiftL` 2) .|. fromEnum side)

pattern PosCurve :: OrientedCurve
pattern PosCurve <- OrientedCurve' ((0b100 .&.) -> 0b100)

pattern NegCurve :: OrientedCurve
pattern NegCurve <- OrientedCurve' ((0b100 .&.) -> 0b000)

{-# COMPLETE OrientedCurve #-}

{-# COMPLETE NegCurve, PosCurve #-}

data OrientedCurveView = OrientedCurveView {sign :: Sign, side :: OpenSide}
  deriving (Show, Eq, Ord, Generic)

viewOC :: OrientedCurve -> OrientedCurveView
viewOC w = OrientedCurveView {sign = sign' w, side = side' w}

sign' :: OrientedCurve -> Sign
{-# INLINE sign' #-}
sign' (OrientedCurve' w) = toEnum $ fromIntegral $ (w .&. 0b100) `shiftR` 2

side' :: OrientedCurve -> OpenSide
{-# INLINE side' #-}
side' (OrientedCurve' w) = OpenSide $ w .&. 0b011

instance Invert OrientedCurve where
  invert = coerce $ xor @Word8 0b100
  {-# INLINE invert #-}

sideL :: Lens' OrientedCurve OpenSide
sideL = lens side' \curve side ->
  OrientedCurve' $ (coerce curve .&. 0b100) .|. fromIntegral (fromEnum side)

instance Rotate OrientedCurve where
  clockwise = sideL %~ clockwise
  counterClockwise = sideL %~ counterClockwise

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

instance HasField "side" OrientedCurve OpenSide where
  getField = side'

instance HasField "sign" OrientedCurve Sign where
  getField = sign'

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
