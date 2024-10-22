package polynomials

import scala.annotation.targetName
import scala.collection.immutable.HashMap

class Polynomial[C: CommutativeRing, V](
    val repr: Map[List[(V, Int)], C]
)(using r: CommutativeRing[C]):
  private def monomialToString(monomial: List[(V, Int)], coeff: C): String =
    val coeffString = coeff match
      case r.unit => ""
      case c      => c.toString()
    coeffString ++ monomial.view.map {
      case (name, 1)   => name.toString()
      case (name, exp) => f"${name}^${exp}"
    }.mkString

  override def toString(): String =
    repr.view.map(monomialToString).mkString(" + ") match
      case "" => "0"
      case s  => s

  override def equals(that: Any): Boolean = that match
    case other: Polynomial[_, _] if other.getClass == this.getClass =>
      this.repr == other.repr
    case _ => false

  /** Evaluates the polynomial with given variable mappings.
    *
    * @tparam R
    *   The type of the ring in which the polynomial is evaluated
    * @param mapping
    *   A function that maps variables to values in R
    * @param convert
    *   An implicit conversion from C to R
    * @return
    *   The result of evaluating the polynomial in R
    */
  def evaluate[R: Ring](
      mapping: V => R
  )(using convert: Conversion[C, R]): R =
    repr.view
      .map((monomilal, coeff) =>
        monomilal
          .map((name, p) => mapping(name) ** p)
          .foldLeft(convert(coeff))(_ * _)
      )
      .foldLeft(Ring[R].zero)(_ + _)

object Polynomial:
  /** Creates a constant polynomial with the given value.
    *
    * @tparam C
    *   The type of the coefficient, must be an element of a commutative ring
    * @tparam V
    *   The type of the variable
    * @param c
    *   The constant value
    * @return
    *   A Polynomial[C, V] representing the constant c
    */
  def const[C, V](c: C)(using r: CommutativeRing[C]): Polynomial[C, V] =
    c match
      case r.zero => Polynomial(Map.empty)
      case c      => Polynomial(Map(List.empty -> c))

  /** Creates a polynomial representing a single variable with coefficient 1.
    *
    * @tparam C
    *   The type of the coefficient, must be an element of a commutative ring
    * @tparam V
    *   The type of the variable
    * @param name
    *   The name or identifier of the variable
    * @return
    *   A Polynomial[C, V] representing the variable with coefficient 1
    */
  def variable[C, V](name: V)(using r: CommutativeRing[C]): Polynomial[C, V] =
    Polynomial(Map(List((name, 1)) -> r.unit))

given polynomialRing[C: CommutativeRing, V]: Ring[Polynomial[C, V]] with
  lazy val zero = Polynomial.const(CommutativeRing[C].zero)
  lazy val unit = Polynomial.const(CommutativeRing[C].unit)
  extension (left: Polynomial[C, V])
    def +(right: Polynomial[C, V]): Polynomial[C, V] =
      val leftMap = left.repr
      val rightMap = right.repr
      val newRepr: Set[(List[(V, Int)], C)] =
        (leftMap.keySet ++ rightMap.keySet)
          .map { monomial =>
            val leftCoeff = leftMap.getOrElse(monomial, CommutativeRing[C].zero)
            val rightCoeff =
              rightMap.getOrElse(monomial, CommutativeRing[C].zero)
            (monomial, (CommutativeRing[C].+(leftCoeff)(rightCoeff)))
          }
          .filter((_, coeff) => coeff != CommutativeRing[C].zero)
      Polynomial(newRepr.toMap)

    def *(right: Polynomial[C, V]): Polynomial[C, V] =
      val leftMap = left.repr
      val rightMap = right.repr
      val elements = for
        (leftMonomial, leftCoeff) <- leftMap
        (rightMonomial, rightCoeff) <- rightMap
      yield {
        val newMonomial =
          (leftMonomial ++ rightMonomial).foldRight(List[(V, Int)]()) {
            case ((curName, curP), (headName, headP) :: tail)
                if curName == headName =>
              (curName, curP + headP) :: tail
            case (cur, acc) => cur :: acc
          }
        Polynomial(
          Map(newMonomial -> CommutativeRing[C].*(leftCoeff)(rightCoeff))
        )
      }

      elements.foldLeft(zero)(_ + _)

  extension (p: Polynomial[C, V])
    def unary_- : Polynomial[C, V] =
      p * Polynomial.const(-CommutativeRing[C].unit)

given [C: CommutativeRing, V]: Conversion[C, Polynomial[C, V]] =
  Polynomial.const(_)

given [T]: Conversion[T, T] = x => x

extension [C: CommutativeRing, V](left: C)(using
    convert: Conversion[C, Polynomial[C, V]]
)
  def +(right: Polynomial[C, V]): Polynomial[C, V] = convert(left) + right
  def *(right: Polynomial[C, V]): Polynomial[C, V] = convert(left) * right
