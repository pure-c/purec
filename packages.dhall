let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2/src/packages.dhall sha256:906af79ba3aec7f429b107fd8d12e8a29426db8229d228c6f992b58151e2308e

let overrides =
      { foreign =
              upstream.foreign
          //  { version = "ad52524676eb2243974010ceebe2cde7a8b8bea7" }
      }

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
            "eee6e976e97765966eceb34d085a80438f32cdb2"
      }

in  upstream // overrides // additions
