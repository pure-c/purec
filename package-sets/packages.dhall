let mkPackage = ./mkPackage.dhall

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2/src/packages.dhall sha256:906af79ba3aec7f429b107fd8d12e8a29426db8229d228c6f992b58151e2308e

let filter =
      https://prelude.dhall-lang.org/List/filter sha256:8ebfede5bbfe09675f246c33eb83964880ac615c4b1be8d856076fdbc4b26ba6

let packages =
      { effect =
              upstream.effect
          //  { repo = "https://github.com/pure-c/purescript-effect.git"
              , version = "c24af908182d08e357185704f489ada6a741b797"
              }
      , prelude =
              upstream.prelude
          //  { repo = "https://github.com/pure-c/purescript-prelude.git"
              , version = "fd751163683c42759258cfaa66fc07a6ac1f26ea"
              }
      , arrays =
              upstream.arrays
          //  { repo = "https://github.com/pure-c/purescript-arrays.git" }
      , assert =
              upstream.assert
          //  { repo = "https://github.com/pure-c/purescript-assert.git"
              , version = "d722b0b1d24640d2bc5df63c6ea5b16357b6667e"
              }
      , bifunctors = upstream.bifunctors
      , console =
              upstream.console
          //  { repo = "https://github.com/pure-c/purescript-console.git"
              , version = "05335e97af6fc9df87d0349d843f7aaf0a9b59f2"
              }
      , control =
              upstream.control
          //  { repo = "https://github.com/pure-c/purescript-control.git"
              , version = "ed59eb9a7ac30e3bb58a671b7e657665b6e8bfb9"
              }
      , distributive = upstream.distributive
      , contravariant = upstream.contravariant
      , const = upstream.const
      , either = upstream.either
      , enums =
              upstream.enums
          //  { repo = "https://github.com/pure-c/purescript-enums.git" }
      , foldable-traversable =
              upstream.foldable-traversable
          //  { repo =
                  "https://github.com/pure-c/purescript-foldable-traversable.git"
              , version = "8649ae8ed0f1abbf1b2ba9c951444aa8344b5906"
              }
      , functions =
              upstream.functions
          //  { repo = "https://github.com/pure-c/purescript-functions.git" }
      , gen = upstream.gen
      , generics-rep = upstream.generics-rep
      , identity = upstream.identity
      , integers =
          { repo = "https://github.com/pure-c/purescript-integers"
          , version = "d30aa8bca51ec8d22722a793739f5e559f017dd5"
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
              , version = "877058fac18aa38a53f500fadf22345d924adf64"
              }
      , maybe = upstream.maybe
      , newtype = upstream.newtype
      , nonempty = upstream.nonempty
      , orders = upstream.orders
      , partial =
              upstream.partial
          //  { repo = "https://github.com/pure-c/purescript-partial.git"
              , version = "58a3db23db423ba08b8f41a0544b239ecbaa7fd4"
              }
      , proxy = upstream.proxy
      , record =
              upstream.record
          //  { repo = "https://github.com/pure-c/purescript-record.git" }
      , refs =
              upstream.refs
          //  { repo = "https://github.com/pure-c/purescript-refs.git"
              , version = "b81d7cab5345b7a5f7f279ad68c4eb7c19c07b0d"
              }
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
          //  { repo = "https://github.com/pure-c/purescript-unfoldable.git"
              , version = "312636383dbd914c8666480a190f47212ccb7e64"
              }
      , unsafe-coerce =
              upstream.unsafe-coerce
          //  { repo = "https://github.com/pure-c/purescript-unsafe-coerce.git"
              , version = "2d8091d065726007ad75c03f0050f73395c91fad"
              }
      , variant = upstream.variant
      }

in  packages
