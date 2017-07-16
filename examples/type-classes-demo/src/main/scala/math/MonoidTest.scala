package math

import effects.Monoid
import Monoid.ops._

object MonoidTest extends App {
  def quickSort[T](xs: List[T])(implicit m: Ordering[T]): List[T] = {
    import m.mkOrderingOps

    xs match {
      case Nil => Nil
      case x :: rest => {
        val (before, after) = rest partition { _ < x }
        quickSort(before) ++ (x :: quickSort(after))
      }
    }
  }

  implicit val intOrdering = Ordering[Int].reverse

  println(
    quickSort(List(5, 1, 2, 3))
  )

  List(2, 3).sum


  def sum[A : Monoid](xs: List[A]) = {
    xs.foldLeft(Monoid[A].identity)(_ |+| _)
  }

  implicit def pairMonoid[A : Monoid, B : Monoid] = new Monoid[(A, B)] {
    override def op(a: (A, B), b: (A, B)): (A, B) =
      (a._1 |+| b._1, a._2 |+| b._2)

    override def identity: (A, B) =
      (Monoid[A].identity, Monoid[B].identity)
  }

//  implicit def mapMonoid[K, V : Monoid[V]] = new Monoid[Map[K, V]]{
//    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = ???
//
//    override def identity: Map[K, V] = Map.empty[K, V]
//  }

  println((2, 3) |+| (4, 5))

//  implicit val ratMonoid = new Monoid[Rational] {
//    override def op(a: Rational, b: Rational): Rational = a * b
//
//    override def identity: Rational = 1
//  }
//
//  println(Rational(4) |+| Rational(2))
//  1 |+| 2
//  "2" |+| "#23"
}
