package effects

import cats.data.NonEmptyList
import math.Semigroup
import math.Semigroup.ops._

sealed trait Validated[+E, +A]

case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]

object Validated {
  type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]

  implicit def validatedApplicative[E : Semigroup] = new Applicative[({type T[A] = Validated[E, A]})#T] {
    def product[A, B](fa: Validated[E, A], fb: Validated[E, B]): Validated[E, (A, B)] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid((a, b))
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
      case (e @ Invalid(_), _) => e
      case (_, e @ Invalid(_)) => e
    }

    def unit[A](a: => A): Validated[E, A] = Valid(a)

    def map[A, B](fa: Validated[E, A])(f: (A) => B): Validated[E, B] = fa match {
      case Valid(a) => Valid(f(a))
      case e @ Invalid(_) => e
    }
  }
}