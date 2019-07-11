let mkPackage = ./mkPackage.dhall

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5/src/packages.dhall sha256:aee7258b1bf1b81ed5e22d1247e812a80ec2e879758562f33334512ed086c5ae

let overrides =
      { effect =
              upstream.effect
          //  { repo = "/home/felix/projects/pure-c/purescript-effect" }
      , prelude =
              upstream.prelude
          //  { repo = "/home/felix/projects/pure-c/purescript-prelude" }
      }

let additions = {=}

in  upstream // overrides // additions
