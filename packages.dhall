let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5/src/packages.dhall sha256:aee7258b1bf1b81ed5e22d1247e812a80ec2e879758562f33334512ed086c5ae

let overrides = {=}

let additions =
      { corefn =
          mkPackage
          [ "console"
          , "foreign-generic"
          , "errors"
          , "strings"
          , "newtype"
          , "tuples"
          , "foldable-traversable"
          , "profunctor"
          , "aff"
          ]
          "https://github.com/felixschl/purescript-corefn.git"
          "compiler/0.12.x"
      }

in  upstream // overrides // additions
