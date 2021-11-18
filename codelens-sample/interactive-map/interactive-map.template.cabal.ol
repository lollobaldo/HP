cabal-version: 1.12

-- This file has been generated from package.yaml by hpack version 0.34.4.
--
-- see: https://github.com/sol/hpack

name:           interactive-map
version:        0.1.0.0

build-type:     Simple
extra-source-files:
    README.md
    ChangeLog.md

library
  exposed-modules:
      Displayable
      Utils
      Main
  build-depends:
      base >=4.7 && <5
    , text
    , mtl
    , bytestring
    , containers
    , diagrams
    , diagrams-lib
    , diagrams-contrib
    , diagrams-svg
    , svg-builder
  default-language: Haskell2010

executable MainDisplay
  main-is:             MainDisplay.hs
  default-language:    Haskell2010
  buildable:           True
  -- ghc-options:         -Wall -fno-warn-unused-do-bind -Werror
  other-modules:
      List
    , Displayable
    , Utils
  build-depends:
        base >=4.7 && <5
    , text
    , mtl
    , bytestring
    , containers
    , diagrams
    , diagrams-lib
    , diagrams-contrib
    , diagrams-svg
    , svg-builder

executable MainEdit
  main-is:             MainEdit.hs
  default-language:    Haskell2010
  buildable:           True
  -- ghc-options:         -Wall -fno-warn-unused-do-bind -Werror
  other-modules:
      List
    , Displayable
    , Utils
  build-depends:
      base >=4.7 && <5
    , text
    , mtl
    , bytestring
    , containers
    , diagrams
    , diagrams-lib
    , diagrams-contrib
    , diagrams-svg
    , svg-builder
