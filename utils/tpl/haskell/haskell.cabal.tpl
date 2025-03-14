cabal-version:      3.12
name:               aoc<<YEAR>>
version:            2021

author:             Ivan Greguric Ortolan
maintainer:         ivan.g.ortolan@gmail.com

library
  hs-source-dirs:      common
  default-language:    Haskell2010

  exposed-modules:
    Common,

  build-depends:
    base     ^>= 4.17,
    parsec   ^>= 3.1.15.0,
    matrix

test-suite doctests
  type:          exitcode-stdio-1.0
  ghc-options:   -threaded
  main-is:       tests/doctests.hs
  default-language:    Haskell2010
  build-depends:
    base,
    doctest        ^>= 0.20,
    directory      ^>= 1.3,
    filepath       ^>= 1.4,

common day
  build-depends:       aoc<<YEAR>>, base
  default-language:    Haskell2010

executable Day01
  import: day
  main-is: days/01/Main.hs
