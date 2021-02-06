let mkPackage = ./mkPackage.dhall

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2/src/packages.dhall sha256:906af79ba3aec7f429b107fd8d12e8a29426db8229d228c6f992b58151e2308e

let filter =
      https://prelude.dhall-lang.org/List/filter sha256:8ebfede5bbfe09675f246c33eb83964880ac615c4b1be8d856076fdbc4b26ba6

let packages =
      { effect =
              upstream.effect
          //  { repo = "https://github.com/pure-c/purescript-effect.git"
              , version = "a68f6898b32e00acf4433b442289d5db2794d526"
              }
      , prelude =
              upstream.prelude
          //  { repo = "https://github.com/pure-c/purescript-prelude.git"
              , version = "202c330c0b313b283573329020e1d75499369b90"
              }
      , arrays =
              upstream.arrays
          //  { repo = "https://github.com/pure-c/purescript-arrays.git" }
      , assert =
              upstream.assert
          //  { repo = "https://github.com/pure-c/purescript-assert.git"
              , version = "1cfc9e24cd6922a46b267da0e056dccb41d855b1"
              }
      , bifunctors = upstream.bifunctors
      , console =
              upstream.console
          //  { repo = "https://github.com/pure-c/purescript-console.git"
              , version = "1928988230d500031a82d0764e4836307af549ed"
              }
      , control =
              upstream.control
          //  { repo = "https://github.com/pure-c/purescript-control.git"
              , version = "ed59eb9a7ac30e3bb58a671b7e657665b6e8bfb9"
              }
      , distributive = upstream.distributive
      , either = upstream.either
      , enums =
              upstream.enums
          //  { repo = "https://github.com/pure-c/purescript-enums.git" }
      , foldable-traversable =
              upstream.foldable-traversable
          //  { repo =
                  "https://github.com/pure-c/purescript-foldable-traversable.git"
              }
      , functions =
              upstream.functions
          //  { repo = "https://github.com/pure-c/purescript-functions.git" }
      , gen = upstream.gen
      , generics-rep = upstream.generics-rep
      , identity = upstream.identity
      , integers =
          { repo = "https://github.com/pure-c/purescript-integers"
          , version = "c"
          , dependencies = [ "math", "maybe", "prelude" ]
          }
      , invariant = upstream.invariant
      , lazy =
              upstream.lazy
          //  { repo = "https://github.com/pure-c/purescript-lazy.git" }
      , lists = upstream.lists
      , math =
              upstream.math
          //  { repo = "https://github.com/pure-c/purescript-math.git"
              , version = "3849291d94c2d88a84e90404acc21e7c71125904"
              }
      , maybe = upstream.maybe
      , newtype = upstream.newtype
      , nonempty = upstream.nonempty
      , orders = upstream.orders
      , partial =
              upstream.partial
          //  { repo = "https://github.com/pure-c/purescript-partial.git"
              , version = "ab22fec1a329806ab4d379bcca96a65681de52ab"
              }
      , proxy = upstream.proxy
      , record =
              upstream.record
          //  { repo = "https://github.com/pure-c/purescript-record.git" }
      , refs =
              upstream.refs
          //  { repo = "https://github.com/pure-c/purescript-refs.git" }
      , st =
              upstream.st
          //  { repo = "https://github.com/pure-c/purescript-st.git" }
      , tailrec = upstream.tailrec
      , transformers = upstream.transformers
      , tuples = upstream.tuples
      , type-equality = upstream.type-equality
      , typelevel-prelude = upstream.typelevel-prelude
      , unfoldable =
              upstream.unfoldable
          //  { repo = "https://github.com/pure-c/purescript-unfoldable.git" }
      , unsafe-coerce =
              upstream.unsafe-coerce
          //  { repo = "https://github.com/pure-c/purescript-unsafe-coerce.git"
              , version = "2d8091d065726007ad75c03f0050f73395c91fad"
              }
      , variant = upstream.variant
      }

in  packages
