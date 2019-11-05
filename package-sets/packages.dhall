let mkPackage = ./mkPackage.dhall

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2/src/packages.dhall sha256:906af79ba3aec7f429b107fd8d12e8a29426db8229d228c6f992b58151e2308e

let filter =
      https://prelude.dhall-lang.org/List/filter sha256:8ebfede5bbfe09675f246c33eb83964880ac615c4b1be8d856076fdbc4b26ba6

let packages =
      { effect =
          /home/felix/projects/pure-c/purescript-effect/spago.dhall as Location
      , prelude =
          /home/felix/projects/pure-c/purescript-prelude/spago.dhall as Location
      , arrays =
              upstream.arrays
          //  { repo = "https://github.com/pure-c/purescript-arrays.git" }
      , assert =
              upstream.assert
          //  { repo = "https://github.com/pure-c/purescript-assert.git"
              , version = "master"
              }
      , bifunctors =
              upstream.bifunctors
          //  { repo = "https://github.com/purescript/purescript-bifunctors.git"
              }
      , console =
              upstream.console
          //  { repo = "https://github.com/pure-c/purescript-console.git" }
      , control =
              upstream.control
          //  { repo = "https://github.com/pure-c/purescript-control.git" }
      , distributive =
              upstream.distributive
          //  { repo =
                  "https://github.com/purescript/purescript-distributive.git"
              }
      , either =
              upstream.either
          //  { repo = "https://github.com/purescript/purescript-either.git" }
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
      , gen =
              upstream.gen
          //  { repo = "https://github.com/purescript/purescript-gen.git" }
      , generics-rep =
              upstream.generics-rep
          //  { repo =
                  "https://github.com/purescript/purescript-generics-rep.git"
              }
      , identity =
              upstream.identity
          //  { repo = "https://github.com/purescript/purescript-identity.git" }
      , integers =
          { repo = "https://github.com/pure-c/purescript-integers"
          , version = "c"
          , dependencies = [ "math", "maybe", "prelude" ]
          }
      , invariant =
              upstream.invariant
          //  { repo = "https://github.com/purescript/purescript-invariant.git"
              }
      , lazy =
              upstream.lazy
          //  { repo = "https://github.com/pure-c/purescript-lazy.git" }
      , lists =
              upstream.lists
          //  { repo = "https://github.com/purescript/purescript-lists.git" }
      , math =
              upstream.math
          //  { repo = "https://github.com/pure-c/purescript-math.git"
              , version = "purescript-integers"
              }
      , maybe =
              upstream.maybe
          //  { repo = "https://github.com/purescript/purescript-maybe.git" }
      , newtype =
              upstream.newtype
          //  { repo = "https://github.com/purescript/purescript-newtype.git" }
      , nonempty =
              upstream.nonempty
          //  { repo = "https://github.com/purescript/purescript-nonempty.git" }
      , orders =
              upstream.orders
          //  { repo = "https://github.com/purescript/purescript-orders.git" }
      , partial =
              upstream.partial
          //  { repo = "https://github.com/pure-c/purescript-partial.git" }
      , proxy =
              upstream.proxy
          //  { repo = "https://github.com/purescript/purescript-proxy.git" }
      , record =
              upstream.record
          //  { repo = "https://github.com/pure-c/purescript-record.git" }
      , refs =
              upstream.refs
          //  { repo = "https://github.com/pure-c/purescript-refs.git" }
      , st =
              upstream.st
          //  { repo = "https://github.com/pure-c/purescript-st.git" }
      , tailrec =
              upstream.tailrec
          //  { repo = "https://github.com/purescript/purescript-tailrec.git" }
      , transformers =
              upstream.transformers
          //  { repo =
                  "https://github.com/purescript/purescript-transformers.git"
              }
      , tuples =
              upstream.tuples
          //  { repo = "https://github.com/purescript/purescript-tuples.git" }
      , type-equality =
              upstream.type-equality
          //  { repo =
                  "https://github.com/purescript/purescript-type-equality.git"
              }
      , typelevel-prelude =
              upstream.typelevel-prelude
          //  { repo =
                  "https://github.com/purescript/purescript-typelevel-prelude.git"
              }
      , unfoldable =
              upstream.unfoldable
          //  { repo = "https://github.com/pure-c/purescript-unfoldable.git" }
      , unsafe-coerce =
              upstream.unsafe-coerce
          //  { repo = "https://github.com/pure-c/purescript-unsafe-coerce.git"
              }
      , variant =
              upstream.variant
          //  { repo = "https://github.com/natefaubion/purescript-variant.git" }
      }

in  packages
