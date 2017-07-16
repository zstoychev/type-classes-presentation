import math.Rational

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object ClassMetainfoDemo extends App {
  // On JVM no generic type info is available at runtime.
  // Scala allows accessing such info through a couple of metainfo type classes like TypeTag,
  // which are automatically created for every concrete type
  def listElementType[A : TypeTag](xs: List[A]) = typeOf[A] match {
    case t if t =:= typeOf[Int] => "List of ints"
    case t if t =:= typeOf[Rational] => "List of rationals"
    case _ => "List of something else"
  }

  listElementType(List(Rational(2), Rational(3))) // List of rationals

  // Does not compile because creation of Java arrays requires metainformation
  // (they are different for primitives and the different kind of objects)
  // def arrayOf[A](seq: A*) = Array[A](seq: _*)

  // ClassTag gives us the information we need for creating arrays
  def arrayOf[A : ClassTag](seq: A*) = Array[A](seq: _*)

  arrayOf(Rational(1), Rational(2))
}
