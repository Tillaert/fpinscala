package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(v) => v
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Branch(l, r) => 1 + depth(l) max depth(r)
      case Leaf(_) => 0
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(v) => f(v)
    }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumFold(t: Tree[Int]): Int =
    fold(t)((x: Int) => x)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l: Int, r: Int) => 1 + l max r)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)):Tree[B])(Branch(_, _))


}