package fpinscala.datastructures

import org.scalatest._

/**
  * Created by Ardjan on 10/06/2016.
  */
class ListSpec extends FlatSpec with Matchers {

  "Exercise 3.1" should "be correct" in {
    def sum[A](l: List[A]): A = sys.error("todo")

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  "Exercise 3.2" should "be correct" in {
    val l = List(1, 2, 3)

    assert(List.tail(l) == List(2, 3))
    assert(List.tail(Nil) == Nil)
  }

  "Exercise 3.3" should "be correct" in {
    val l = List(1, 2, 3)

    assert(List.setHead(l, 4) == List(4, 2, 3))
    assert(List.setHead(Nil, 4) == Nil)
  }

  "Exercise 3.4" should "be correct" in {
    val l = List(1, 2, 3, 4, 5, 6)

    assert(List.drop(l, 0) == l)
    assert(List.drop(l, -1) == l)
    assert(List.drop(l, 2) == List(3, 4, 5, 6))
    assert(List.drop(l, 4) == List(5, 6))
    assert(List.drop(l, 6) == Nil)
    assert(List.drop(l, 7) == Nil)
  }

  "Exercise 3.5" should "drop while correctly" in {
    val l = List[Boolean](true, true, true)
    val l2 = List(true, true, false)
    val l3 = List(1, 2, 3, 4)

    assert(List.dropWhile[Boolean](l, x => x) == Nil)
    assert(List.dropWhile[Boolean](l2, x => x) == List(false))
    assert(List.dropWhile[Int](l3, _ < 3) == List(3, 4))
  }

  "Exercise 3.6" should "init correctly" in {
    val l = List(1)
    val l2 = List(1, 2, 3, 4)


    assert(List.init(Nil) == Nil)
    assert(List.init(l) == Nil)
    assert(List.init(l2) == List(1, 2, 3))

  }

  "Exercise 3.8" should "reproduce original list" in {
    val l = List(1, 2, 3, 4, 5, 6)

    assert(List.foldRight(l, Nil: List[Int])(Cons(_, _)) == l)
  }

  "Exercise 3.9" should "return length" in {
    val l = List(1, 2, 3, 4, 5)

    assert(List.length(Nil) == 0)
    assert(List.length(l) == 5)
  }

  "Exercise 3.10" should "foldLeft" in {
    val l = List(1,20,300,4000)

    assert( List.foldLeft(l,50000)(_+_) == 54321 )
  }

  "Exercise 3.11" should "sum via foldleft" in {
    val l = List(1,20,300,4000)

    assert( List.sumViaFoldLeft(l) == 4321 )
    assert( List.sumViaFoldLeft(Nil) == 0 )
  }

  "Exercise 3.11" should "product via foldleft" in {
    val l = List(1,2,3,4)

    assert( List.productViaFoldLeft(l) == 24 )
    assert( List.productViaFoldLeft(Nil) == 1 )
  }

  "Exercise 3.11" should "length via foldleft" in {
    val l = List(1,2,3,4)

    assert( List.lengthViaFoldLeft(l) == 4 )
    assert( List.lengthViaFoldLeft(Nil) == 0 )
  }

  "Exercise 3.12" should "reverse" in {
    assert( List.reverse(Nil) == Nil)
    assert( List.reverse(List(1,2,3,4)) == List(4,3,2,1))
  }

  "Exercise 3.13" should "foldRight as foldLeft" in {
    val l = List(1,2,3,4,5)

    assert( List.foldRightViaFoldLeft(l, Nil:List[Int])(Cons(_,_)) == l )
  }
}
