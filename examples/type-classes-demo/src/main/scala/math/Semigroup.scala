package math

trait Semigroup[M] {
  def op(a: M, b: M): M
}

object Semigroup {
  def apply[A](implicit m: Semigroup[A]): Semigroup[A] = m

  object ops {
    implicit class SemigroupOps[A](a: A)(implicit m: Semigroup[A]) {
      def |+|(b: A) = m.op(a, b)
    }
  }
}