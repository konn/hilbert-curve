# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build**: `cabal build`
- **Test**: `cabal test` (runs both unit tests via tasty-discover and doctests)
- **Run executable**: `cabal run hilbert-curves-exe`
- **REPL**: `cabal repl` or `cabal repl hilbert-curves` for the library
- **Format code**: `fourmolu --mode inplace src/ app/ test/ doctest/`
- **Clean**: `cabal clean`

## Test Commands

- **Run all tests**: `cabal test`
- **Run unit tests only**: `cabal test hilbert-curves-test`
- **Run doctests only**: `cabal test hilbert-curves-doctest`

## Code Architecture

This is a Haskell library implementing Hilbert space-filling curves with visualization capabilities using the diagrams library.

### Core Module Structure

- `Math.Curve.SurfaceFilling.Hilbert`: Main library module containing the Hilbert curve implementation
  - Core types: `Cell`, `Pos` (UpLeft/DownLeft/UpRight/DownRight), `Axis` (Vertical/Horizontal), `Dir` (Straight/Turn)
  - `hilbert0`: Base Hilbert curve cell
  - `Rotate` typeclass for geometric transformations
  - Uses generic-lens for record field access with OverloadedLabels

### Dependencies

- **Core**: base, lens, generic-lens, linear
- **Visualization**: diagrams, diagrams-rasterific  
- **Testing**: tasty (with tasty-discover), doctest-parallel

### Code Style

- Uses GHC2021 language extension set
- Fourmolu formatting with 2-space indentation
- NoFieldSelectors with OverloadedLabels for record access
- Strict fields marked with `!` and `{-# UNPACK #-}`
- Comprehensive GHC warning flags enabled

### Testing Strategy

- Unit tests auto-discovered via tasty-discover in `test/`
- Doctests run via doctest-parallel
- Both test suites configured with threading and RTS options

## Development Workflow

- After you changed Haskell code, run `cabal build all` and `cabal test all` for sanity check.