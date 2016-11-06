import fpinscala.iomonad.Monad

import scala.language.higherKinds

object FreeWS {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)

      override def flatMap[A, B](a: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = a flatMap (f)
    }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(v) => v
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(v) => runTrampoline[A] {
          f(v)
        }
        case Suspend(r) => runTrampoline[A] {
          f(r())
        }
        case FlatMap(a0, g) => runTrampoline[A] {
          a0.flatMap(a0 => g(a0).flatMap(f))
        }
      }
    }
}



