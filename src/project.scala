package polynomials

import scala.language.implicitConversions

@main def main(): Unit =
  import Polynomial.variable

  val x: Polynomial[Int, "x"] = variable("x")

  val mat = Matrix[Int, 3](
    Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
  )

  val mat2 = Matrix[Int, 2](
    Array(
      Array(1, 2),
      Array(4, 5)
    )
  )

  val poly = x ** 2 + 3

  println(poly.evaluate(_ => mat))
