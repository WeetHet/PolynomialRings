import scala.reflect.ClassTag

class Matrix[T, S <: Int](
    repr: Array[Array[T]]
)(using s: ValueOf[S]):
  val size: Int = s.value
  assert(repr.size == size && repr.forall(_.size == size))

  def apply(i: Int, j: Int): T = repr(i)(j)

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

object Matrix:
  def fromList[T: ClassTag, S <: Int](list: List[List[T]])(using
      s: ValueOf[S]
  ): Matrix[T, S] = Matrix(list.map(_.toArray).toArray)

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
    def +(right: Matrix[T, S]): Matrix[T, S] = Matrix(
      Array.tabulate(s.value, s.value)((i, j) =>
        ring.+(left(i, j))(right(i, j))
      )
    )

    def *(right: Matrix[T, S]): Matrix[T, S] = Matrix(
      Array.tabulate(s.value, s.value)((i, j) =>
        (0 until s.value)
          .map(k => ring.*(left(i, k))(right(k, j)))
          .reduce(ring.+(_)(_))
      )
    )

  extension (p: Matrix[T, S])
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
