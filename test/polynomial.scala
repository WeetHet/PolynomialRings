package tests

import scala.language.implicitConversions

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import polynomials.{Polynomial, Ring, Matrix, given}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Gen.{listOfN, oneOf}
import scala.concurrent.duration.Duration

type Poly = Polynomial[Int, "x" | "y"]

val x: Poly = Polynomial.variable("x")
val y: Poly = Polynomial.variable("y")

val monomialsList: List[Poly] =
  List(summon[Ring[Poly]].unit, x, y) ++ List(
    x * x,
    x * y,
    y * x,
    y * y
  )

implicit lazy val arbitraryIntPolynomial
    : Arbitrary[Polynomial[Int, "x" | "y"]] = Arbitrary(
  listOfN(monomialsList.size, arbInt.arbitrary)
    .map(
      _.zip(monomialsList)
        .map((c, p) => Polynomial.const[Int, "x" | "y"](c) * p)
        .foldLeft(Ring[Poly].zero)(_ + _)
    )
)

class PolyRingTest extends ScalaCheckSuite:
  val polyRing = polynomialRing[Int, "x" | "y"]
  import polyRing.{zero, unit}

  property("polynomial ring zero is identity of addition"):
    forAll: (x: Poly) =>
      zero + x == x

  property("polynomial ring unit is identity of multiplication"):
    forAll: (x: Poly) =>
      unit * x == x

  property("polynomial ring has inverse"):
    forAll: (a: Poly) =>
      a + (-a) == zero

  property("polynomial ring addition is commutative"):
    forAll: (a: Poly, b: Poly) =>
      a + b == b + a

  property("polynomial ring addition is associative"):
    forAll: (a: Poly, b: Poly, c: Poly) =>
      (a + b) + c == a + (b + c)

  property("polynomial ring multiplication is associative"):
    forAll: (a: Poly, b: Poly, c: Poly) =>
      (a * b) * c == a * (b * c)

  property("polynomial ring multiplication is left distributive"):
    forAll: (a: Poly, b: Poly, c: Poly) =>
      a * (b + c) == a * b + a * c

  property("polynomial ring multiplication is right distributive"):
    forAll: (a: Poly, b: Poly, c: Poly) =>
      (b + c) * a == b * a + c * a

class PolyMatrixEvalTest extends munit.FunSuite:
  test("polynomial evalution with matrices"):
    val x: Polynomial[Int, "x"] = Polynomial.variable("x")

    val mat = Matrix[Int, 3](
      List(
        List(1, 2, 3),
        List(4, 5, 6),
        List(7, 8, 9)
      )
    )

    val poly = x ** 2 + 3
    assertEquals(
      poly.evaluate(_ => mat),
      Matrix[Int, 3](
        List(
          List(33, 36, 42),
          List(66, 84, 96),
          List(102, 126, 153)
        )
      )
    )
