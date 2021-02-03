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
              , version = "f9fa363b63240dae99109a95c8007c76a4f32d4c"
              }
      , arrays =
              upstream.arrays
          //  { repo = "https://github.com/pure-c/purescript-arrays.git" }
      , assert =
              upstream.assert
          //  { repo = "https://github.com/pure-c/purescript-assert.git"
              , version = "master"
              }
      , bifunctors = upstream.bifunctors
      , console =
              upstream.console
          //  { repo = "https://github.com/pure-c/purescript-console.git" }
      , control =
              upstream.control
          //  { repo = "https://github.com/pure-c/purescript-control.git" }
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
              , version = "purescript-integers"
              }
      , maybe = upstream.maybe
      , newtype = upstream.newtype
      , nonempty = upstream.nonempty
      , orders = upstream.orders
      , partial =
              upstream.partial
          //  { repo = "https://github.com/pure-c/purescript-partial.git" }
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
              }
      , variant = upstream.variant
      }

in  packages
