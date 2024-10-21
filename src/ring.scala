trait Ring[T]:
  lazy val zero: T
  lazy val unit: T

  extension (left: T)
    def +(right: T): T
    def -(right: T): T = left + (-right)
    def *(right: T): T
  extension (p: T) def unary_- : T

object Ring:
  def apply[T: Ring]: Ring[T] = summon[Ring[T]]

extension [T](left: T)(using r: Ring[T])
  def **(right: Int): T =
    var curExp = right
    var current = left
    var ans = r.unit
    while curExp != 0 do
      if curExp % 2 == 1 then ans *= current
      current *= current
      curExp = curExp / 2
    ans

trait CommutativeRing[T] extends Ring[T]

object CommutativeRing:
  def apply[T: CommutativeRing]: CommutativeRing[T] = summon[CommutativeRing[T]]

given [T](using n: Numeric[T]): CommutativeRing[T] with
  lazy val zero: T = n.zero
  lazy val unit: T = n.one
  extension (left: T)
    def +(right: T): T = n.plus(left, right)
    def *(right: T): T = n.times(left, right)

  extension (p: T) def unary_- : T = n.negate(p)
