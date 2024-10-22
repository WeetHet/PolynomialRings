//> using test.dep org.scalameta::munit::1.0.2
//> using test.dep org.scalameta::munit-scalacheck::1.0.0

package tests

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.{listOfN}
import polynomials.{Ring, CommutativeRing, Matrix, given}
import scala.reflect.ClassTag
import org.scalacheck.Gen

implicit lazy val arbitrary3x3IntMatrix: Arbitrary[Matrix[Int, 3]] = Arbitrary(
  listOfN(3, listOfN(3, Arbitrary.arbInt.arbitrary).map(_.toArray))
    .map(list => Matrix[Int, 3](list.toArray))
)

class MatrixRingTest extends ScalaCheckSuite:
  val matrixRing = summon[Ring[Matrix[Int, 3]]]
  import matrixRing.{unit, zero}

  property("matrix ring zero is identity of addition"):
    forAll: (x: Matrix[Int, 3]) =>
      zero + x == x

  property("matrix ring unit is identity of multiplication"):
    forAll: (x: Matrix[Int, 3]) =>
      unit * x == x

  property("matrix ring has inverse"):
    forAll: (a: Matrix[Int, 3]) =>
      a + (-a) == zero

  property("matrix ring addition is commutative"):
    forAll: (a: Matrix[Int, 3], b: Matrix[Int, 3]) =>
      a + b == b + a

  property("matrix ring addition is associative"):
    forAll: (a: Matrix[Int, 3], b: Matrix[Int, 3], c: Matrix[Int, 3]) =>
      (a + b) + c == a + (b + c)

  property("matrix ring multiplication is associative"):
    forAll: (a: Matrix[Int, 3], b: Matrix[Int, 3], c: Matrix[Int, 3]) =>
      (a * b) * c == a * (b * c)

  property("matrix ring multiplication is left distributive"):
    forAll: (a: Matrix[Int, 3], b: Matrix[Int, 3], c: Matrix[Int, 3]) =>
      a * (b + c) == a * b + a * c

  property("matrix ring multiplication is right distributive"):
    forAll: (a: Matrix[Int, 3], b: Matrix[Int, 3], c: Matrix[Int, 3]) =>
      (b + c) * a == b * a + c * a
