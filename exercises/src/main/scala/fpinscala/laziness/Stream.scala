package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    this.foldRight(Nil: List[A])((v, l) => v :: l)

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](l: => Stream[B]): Stream[B] =
    foldRight(l)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile{ _._2.isDefined }.forAll{ case (l,r) => l == r }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), v) if v > 0 => Some(h(), (t(), v - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some(f(hl(), hr()), (tl(), tr()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(hl, tl), Cons(hr, tr)) => Some((Some(hl()), Some(hr())), (tl(), tr()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some((s,s.drop(1)))
    } append Stream(empty)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      Stream.cons(n1, go(n2, n1 + n2))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((v, s)) => Stream.cons(v, unfold(s)(f))
      case _ => empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold[Int, (Int, Int)](0, 1) { case (n1, n2) => Some((n1, (n2, n1 + n2))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold[Int, Int](n)(v => Some(v, v + 1))

  def constantViaUnfold(n: Int): Stream[Int] =
    unfold[Int, Int](n)(_ => Some(n, n))

  def onesViaUnfold: Stream[Int] =
    unfold[Int, Int](1)(_ => Some(1, 1))


}