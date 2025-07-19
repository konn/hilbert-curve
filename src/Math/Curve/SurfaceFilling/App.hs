{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Math.Curve.SurfaceFilling.App (
  defaultMain,
  defaultMainWith,
  Curve (..),
  Options (..),
  optionsP,
) where

import Control.Applicative
import Diagrams (mkSizeSpec2D)
import Diagrams.Backend.Rasterific (renderRasterific)
import GHC.Generics (Generic)
import Math.Curve.SurfaceFilling.Hilbert (drawHilbertCurve)
import Options.Applicative qualified as Opt
import System.Directory
import System.FilePath
import Text.Printf

data Curve = Hilbert
  deriving (Show, Eq, Ord, Generic)

data Options = Options
  { curve :: !Curve
  , size :: !Int
  , outputDir :: !FilePath
  , iteration :: !Word
  }
  deriving (Show, Eq, Ord, Generic)

optionsP :: Opt.ParserInfo Options
optionsP =
  Opt.info
    (p <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Generate an image of surface-filling curve"
        <> Opt.header "Hilbert Curve Generator"
    )
  where
    p :: Opt.Parser Options
    p = do
      curve <-
        Opt.hsubparser $
          Opt.command "hilbert" (Opt.info (pure Hilbert) (Opt.progDesc "Hilbert curve"))
      size <-
        Opt.option
          Opt.auto
          ( Opt.long "size"
              <> Opt.short 's'
              <> Opt.value 128
              <> Opt.showDefault
              <> Opt.help "Size of the curve"
          )
      outputDir <-
        Opt.strOption
          ( Opt.long "output-dir"
              <> Opt.short 'o'
              <> Opt.value "."
              <> Opt.showDefault
              <> Opt.help "Output directory for the generated image"
          )
      iteration <-
        Opt.option
          Opt.auto
          ( Opt.long "iteration"
              <> Opt.short 'n'
              <> Opt.value 5
              <> Opt.showDefault
              <> Opt.help "Number of iterations for the curve (default: 5)"
          )
      pure Options {..}

defaultMain :: IO ()
defaultMain =
  defaultMainWith
    =<< Opt.customExecParser
      (Opt.prefs Opt.subparserInline)
      optionsP

defaultMainWith :: Options -> IO ()
defaultMainWith Options {..} = do
  let size' = Just $ fromIntegral size
  createDirectoryIfMissing True outputDir
  let dest = outputDir </> printf "%s-%02d-%dx%d.png" (curveName curve) iteration size size
  renderRasterific dest (mkSizeSpec2D size' size') $
    drawHilbertCurve iteration
  putStrLn $ "Generated image: " <> dest

curveName :: Curve -> String
curveName Hilbert = "hilbert"
