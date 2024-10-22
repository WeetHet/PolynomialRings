package polynomials

import scala.reflect.ClassTag

/** Represents a square matrix of size S x S with elements of type T.
  *
  * @tparam T
  *   The type of elements in the matrix
  * @tparam S
  *   A singleton integer type representing the size of the matrix
  * @param repr
  *   The underlying 2D array representation of the matrix
  */
class Matrix[T: ClassTag, S <: Int](
    private val repr: Array[Array[T]]
)(using s: ValueOf[S]):
  val size: Int = s.value
  assert(repr.size == size && repr.forall(_.size == size))

  def apply(i: Int, j: Int): T = repr(i)(j)

  override def equals(that: Any): Boolean = that match
    case other: Matrix[_, _] if other.getClass == this.getClass =>
      size == other.size && repr.view
        .zip(other.repr.view)
        .forall(_.zip(_).forall(_ == _))
    case _ => false

  override def toString(): String =
    val maxSize = (0 until size)
      .map(j => (0 until size).map(i => repr(i)(j).toString().length()).max)
      .toArray

    repr.zipWithIndex
      .map((row, i) =>
        val rowRepr = row.zipWithIndex
          .map((x, col) =>
            x.toString().reverse.padTo(maxSize(col), ' ').reverse
          )
          .mkString(" ")
        val (s, e) = i match
          case 0                  => ("⎛", "⎞")
          case x if x == size - 1 => ("⎝", "⎠")
          case _                  => ("⎜", "⎜")

        s"$s $rowRepr $e"
      )
      .mkString("\n")

given matrixRing[T: ClassTag, S <: Int](using
    s: ValueOf[S],
    ring: Ring[T]
): Ring[Matrix[T, S]] with
  lazy val zero: Matrix[T, S] = Matrix(
    Array.fill(s.value, s.value)(ring.zero)
  )

  lazy val unit: Matrix[T, S] = Matrix(
    Array.tabulate(s.value, s.value)((i, j) =>
      if (i == j) ring.unit else ring.zero
    )
  )

  extension (left: Matrix[T, S])
    /** Adds two matrices element-wise.
      *
      * @param left
      *   The first matrix to add
      * @param right
      *   The second matrix to add
      * @return
      *   A new matrix resulting from the element-wise addition
      */
    def +(right: Matrix[T, S]): Matrix[T, S] = Matrix(
      Array.tabulate(s.value, s.value)((i, j) =>
        ring.+(left(i, j))(right(i, j))
      )
    )

    /** Multiplies two matrices.
      *
      * @param left
      *   The left matrix to multiply
      * @param right
      *   The right matrix to multiply
      * @return
      *   A new matrix resulting from the multiplication of left and right
      */
    def *(right: Matrix[T, S]): Matrix[T, S] = Matrix(
      Array.tabulate(s.value, s.value)((i, j) =>
        (0 until s.value)
          .map(k => ring.*(left(i, k))(right(k, j)))
          .reduce(ring.+(_)(_))
      )
    )

  extension (p: Matrix[T, S])
    /** Negates the matrix, returning a new matrix with all elements negated.
      *
      * @param p
      *   The matrix to negate
      * @return
      *   A new matrix with all elements negated
      */
    def unary_- : Matrix[T, S] = Matrix(
      Array.tabulate(s.value, s.value)((i, j) => -p(i, j))
    )

given [T: ClassTag, S <: Int](using
    s: ValueOf[S],
    ring: Ring[T]
): Conversion[T, Matrix[T, S]] with
  def apply(x: T): Matrix[T, S] = Matrix(
    Array.tabulate(s.value, s.value)((i, j) =>
      if (i == j) (x * ring.unit) else ring.zero
    )
  )
