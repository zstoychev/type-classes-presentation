package solutions

import Monoid.ops._

object Sum extends App {
  def sum[T](xs: List[T])(implicit m: Monoid[T]) = {
    xs.foldLeft(m.identity)(m.op)
  }

  def sum2[T : Monoid](xs: List[T]) = {
    xs.foldLeft(Monoid[T].identity)(_ |+| _)
  }

  println(sum(List(3, 5, 9, 1)))
  println(sum2(List(3, 5, 9, 1)))

  println(1 |+| 2)

  println((3, 4) |+| (5, 6))

  println(sum(List.empty[(Int, Int)]))

  val map1 = Map(1 -> (2, 3), 2 -> (3, 4))
  val map2 = Map(2 -> (5, 6), 3 -> (7, 8))

  println(map1 |+| map2)
}
