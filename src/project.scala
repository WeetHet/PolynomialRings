//> using dep dev.zio::izumi-reflect::2.3.10

package polynomials

import scala.language.implicitConversions

@main def main(): Unit =
  import Polynomial.variable

  val x: Polynomial[Int, "x"] = variable[Int, "x"]("x")

  val mat = Matrix[Int, 3](
    Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
  )

  val poly = x ** 2 + 3

  println(poly.evaluate(_ => mat))
