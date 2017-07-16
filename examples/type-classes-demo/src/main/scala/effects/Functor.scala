package effects

import scala.language.higherKinds

trait Functor[F[_]]  {
  def map[A, B](m: F[A])(f: A => B): F[B]
}
