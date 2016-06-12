package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, t) => t
      case _ => Nil
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(_, t) => Cons(h, t)
      case _ => Nil
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, t) if (n > 0) => drop(t, n - 1)
      case _ => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f)
      case _ => l
    }


  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case _ => Nil
    }


  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, n) => n + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumViaFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]) = foldLeft[A, Int](l, 0)((l, _) => l + 1)

  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((l, v) => Cons(v, l))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])( (h,t) => Cons(f(h),t))

  def appendViaFolding[A](l: List[A], k: List[A]): List[A] =
    List.foldRight(l, k)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] =
    List.foldRight(l, Nil: List[A])(append)

  def plusone(l: List[Int]): List[Int] =
    List.foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def mapToString(l: List[Double]) =
    List.foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    List.foldRight(l, Nil: List[A])( (h,t) => if (f(h)) Cons(h,t) else t )

  def flatMap[A,B](as:List[A])(f: A => List[B]): List[B] =
    List.flatten(List.map(as)(f))

  def filterViaFlatmap[A](l: List[A])(f: A => Boolean): List[A] =
    List.flatMap(l)(i => if (f(i)) List(i) else Nil )

  def addLists(l: List[Int], r:List[Int]) : List[Int] =
    (l,r) match {
      case (Cons(hl, tl), Cons(hr, tr)) => Cons(hl + hr, addLists(tl, tr))
      case _ => Nil
    }

  def zipWith[A,B,C]( l: List[A], r: List[B] ) (f: (A,B) => C ): List[C] =
    (l,r) match {
      case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl,hr), zipWith(tl,tr)(f))
      case _ => Nil
    }

  @annotation.tailrec
  def startsWith[A](l:List[A], sub:List[A]):Boolean =
    (l, sub) match {
      case (Cons(hl,_), Cons(hr,_)) if(hl != hr) => false
      case (Cons(_,tl), Cons(_,tr)) => startsWith(tl,tr)
      case _ => true
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => false
      case Cons(_,t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    }




}
