# Polynomial implementation in Scala

- `ring.scala` &mdash; ring and commutative ring typeclasses
- `matrix.scala` &mdash; matrix implementation
- `polynomial.scala` &mdash; polynomial implementation. Polynomial coefficents should belong to a commutative ring.
  To evaluate a polynomial you have to give a mapping from variables to the evaluation type as well as a way to convert coefficients to the result type

Run with `scala-cli run . -language:strictEquality`

Devenv is provided via [nix](nixos.org), run `nix develop` or `direnv allow`
