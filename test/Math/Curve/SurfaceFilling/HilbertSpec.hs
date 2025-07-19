{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -dsuppress-all -dno-suppress-type-signatures #-}

module Math.Curve.SurfaceFilling.HilbertSpec (
  module Math.Curve.SurfaceFilling.HilbertSpec,
) where

import Math.Curve.SurfaceFilling.Hilbert
import Test.Tasty
import Test.Tasty.Inspection

trailUP :: [Quadrant]
trailUP = curveTrail OrientedCurve {sign = P, side = U}

trailUPExpect :: [Quadrant]
trailUPExpect = [UL, DL, DR, UR]

test_trail :: TestTree
test_trail =
  testGroup
    "trail"
    [ $(inspectTest $ 'trailUP ==~ 'trailUPExpect)
    -- , $(inspectTest $ hasNoTypeClasses 'trailUN)
    -- , $(inspectTest $ hasNoTypeClasses 'trailLP)
    -- , $(inspectTest $ hasNoTypeClasses 'trailLN)
    -- , $(inspectTest $ hasNoTypeClasses 'trailDP)
    -- , $(inspectTest $ hasNoTypeClasses 'trailDN)
    -- , $(inspectTest $ hasNoTypeClasses 'trailRP)
    -- , $(inspectTest $ hasNoTypeClasses 'trailRN)
    ]
