package effects

import effects.Applicative.ApplicativeOps
import effects.Monad.MonadOps

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def unit[A](a: => A): F[A]

  def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def join[A](mm: F[F[A]]): F[A] = flatMap(mm)(x => x)
}

object Monad {
  def apply[F[_]](implicit m: Monad[F]) = m

  trait MonadOps extends ApplicativeOps {
    implicit class MonadOps[F[_] : Monad, A](fa: F[A]) {
      def flatMap[B](f: A => F[B]) = Monad[F].flatMap(fa)(f)

//      // Scala typed fun
//      def join[B](implicit ev: A =:= F[B]): F[B] = Monad[F].join(ev(fa))
    }
  }
  object ops extends MonadOps

  // Implementing monads from the Scala library
  implicit val optionMonad = new MonadWithZero[Option] {
    def flatMap[A, B](m: Option[A])(f: A => Option[B]): Option[B] = m flatMap f
    def unit[A](a: => A): Option[A] = Some(a)
    def mzero[A]: Option[A] = None

    override def map[A, B](m: Option[A])(f: A => B): Option[B] = m map f
  }

  implicit val tryMonad = new MonadWithZero[Try] {
    def flatMap[A, B](m: Try[A])(f: A => Try[B]): Try[B] = m flatMap f
    def unit[A](a: => A): Try[A] = Success(a)
    def mzero[A]: Try[A] = Failure(new NoSuchElementException)

    override def map[A, B](m: Try[A])(f: A => B): Try[B] = m map f
  }

  implicit val listMonad = new MonadWithZero[List] {
    def flatMap[A, B](m: List[A])(f: (A) => List[B]): List[B] = m flatMap f
    def unit[A](a: => A): List[A] = List(a)
    def mzero[A]: List[A] = Nil

    override def map[A, B](m: List[A])(f: A => B): List[B] = m map f
  }

  implicit def futureMonad(implicit ec: ExecutionContext) = new MonadWithZero[Future] {
    def flatMap[A, B](m: Future[A])(f: (A) => Future[B]): Future[B] = m flatMap f
    def unit[A](a: => A): Future[A] = Future(a)
    def mzero[A]: Future[A] = Future.failed(new NoSuchElementException)

    override def map[A, B](m: Future[A])(f: A => B): Future[B] = m map f
  }
}

trait MonadWithZero[F[_]] extends Monad[F] {
  def mzero[A]: F[A]
  def filter[A](fa: F[A])(f: A => Boolean): F[A] = flatMap(fa) { x => if (f(x)) unit(x) else mzero }
}

object MonadWithZero {
  def apply[F[_]](implicit m: MonadWithZero[F]) = m

  trait MonadWithZeroOps extends MonadOps {
    implicit class MonadWithZeroOps[F[_] : MonadWithZero, A](fa: F[A]) {
      def filter(f: A => Boolean) = MonadWithZero[F].filter(fa)(f)
    }
  }
  object ops extends MonadWithZeroOps
}
