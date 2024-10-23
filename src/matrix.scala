package polynomials

import scala.collection.SeqView

/** Represents a square matrix of size S x S with elements of type T.
  *
  * @tparam T
  *   The type of elements in the matrix
  * @tparam S
  *   A singleton integer type representing the size of the matrix
  * @param repr
  *   The underlying 2D array representation of the matrix
  */
case class Matrix[T: Eq, S <: Int](
    private val repr: List[List[T]]
)(using s: ValueOf[S]):
  val size: Int = s.value
  assert(repr.size == size && repr.forall(_.size == size))

  def asRows: List[List[T]] = repr
  def asCols: List[List[T]] = asRows.transpose

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

given [T: Eq, S <: Int]: CanEqual[Matrix[T, S], Matrix[T, S]] =
  CanEqual.derived

given matrixRing[T: Eq, S <: Int](using
    s: ValueOf[S],
    ring: Ring[T]
): Ring[Matrix[T, S]] with
  lazy val zero: Matrix[T, S] = Matrix(
    List.fill(s.value, s.value)(ring.zero)
  )

  lazy val unit: Matrix[T, S] = Matrix(
    List.tabulate(s.value, s.value)((i, j) =>
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
      left.asRows.zip(right.asRows).map(_.zip(_).map(ring.+(_)(_)))
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
      left.asRows.map(row =>
        right.asCols.map(col =>
          row
            .zip(col)
            .map(ring.*(_)(_))
            .foldLeft(ring.zero)(ring.+(_)(_))
        )
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
      p.asRows.map(_.map(ring.unary_-))
    )

given [T: Eq, S <: Int](using
    s: ValueOf[S],
    ring: Ring[T]
): Conversion[T, Matrix[T, S]] with
  def apply(x: T): Matrix[T, S] = Matrix(
    List.tabulate(s.value, s.value)((i, j) =>
      if (i == j) (x * ring.unit) else ring.zero
    )
  )
