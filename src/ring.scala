package polynomials

/** A trait representing a mathematical ring structure.
  *
  * @tparam T
  *   The type of elements in the ring
  */
trait Ring[T]:
  /** The additive identity element of the ring */
  lazy val zero: T

  /** The multiplicative identity element of the ring */
  lazy val unit: T

  extension (left: T)
    /** Addition operation in the ring */
    def +(right: T): T

    /** Subtraction operation in the ring
      *
      * Default implementation: a - b = a + (-b)
      */
    def -(right: T): T = left + (-right)

    /** Multiplication operation in the ring */
    def *(right: T): T

  extension (p: T)
    /** Unary negation operation in the ring */
    def unary_- : T

object Ring:
  def apply[T: Ring]: Ring[T] = summon[Ring[T]]

trait RingPow[T: Ring]:
  extension (left: T)
    /** Exponentiation operation for ring elements
      *
      * Raises a ring element to an integer power
      *
      * @param right
      *   The exponent (must be non-negative)
      * @return
      *   The result of left raised to the power of right
      */
    def **(right: Int): T

given binaryExp[T: Ring]: RingPow[T] with
  extension (left: T)
    def **(right: Int): T =
      assert(right >= 0)
      var curExp = right
      var current = left
      var ans = summon[Ring[T]].unit
      while curExp != 0 do
        if curExp % 2 == 1 then ans *= current
        current *= current
        curExp = curExp / 2
      ans

trait CommutativeRing[T] extends Ring[T]

object CommutativeRing:
  def apply[T: CommutativeRing]: CommutativeRing[T] = summon[CommutativeRing[T]]

/** Given instance for CommutativeRing[T] using Numeric[T]
  *
  * This instance allows any type T with a Numeric instance to be treated as a
  * CommutativeRing. Scala actually doesn't specify that Numeric multiplication
  * should be commutative, but this is true for the standard library and for the
  * sake of simplicity we will treat it as such
  *
  * @tparam T
  *   The type of elements in the ring
  * @param n
  *   The Numeric instance for T
  * @return
  *   A CommutativeRing instance for T
  */
given numericRing[T](using n: Numeric[T]): CommutativeRing[T] with
  lazy val zero: T = n.zero
  lazy val unit: T = n.one
  extension (left: T)
    def +(right: T): T = n.plus(left, right)
    def *(right: T): T = n.times(left, right)

  extension (p: T) def unary_- : T = n.negate(p)
