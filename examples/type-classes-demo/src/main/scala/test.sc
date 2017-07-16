import effects.Monoid
import math.Rational

def sum[A](xs: List[A])(m: Monoid[A]) = {
  xs.foldLeft(m.identity)(m.op)
}


sum(List(Rational(2), Rational(3)))(Rational.rationalMonoid)