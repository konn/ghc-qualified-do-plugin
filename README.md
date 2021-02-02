# ghc-qualified-do-plugin
GHC Source Plugin to emulate `QualifiedDo` extension in GHC <9.0.

Due to how GHC parse expressions, this gives slightly different syntax than in GHC >= 9.0.
In particular, `M.do` must be written `M . do`, `M. do`, or `M .do` to parse it as an infix application of `(.)` to `M` and `do`; this is because Source Plugin can treat only valid Haskell input.
