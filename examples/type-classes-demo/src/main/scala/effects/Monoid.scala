package effects

trait Monoid[M] {
  def op(a: M, b: M): M
  def identity: M
}

object Monoid {
  def apply[A](implicit m: Monoid[A]) = m

  object ops {
    implicit class monoidOp[A](a: A)(implicit m: Monoid[A]) {
      def |+|(b: A) = m.op(a, b)
    }
  }

  implicit val intMonoid = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def identity: Int = 0
  }

  implicit val stringMonoid = new Monoid[String] {
    override def op(a: String, b: String): String = a + b
    override def identity: String = ""
  }
}