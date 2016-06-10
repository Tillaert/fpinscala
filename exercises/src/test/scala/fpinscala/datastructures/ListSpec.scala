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
      case Cons(h,t) => h + sum (t)
      case _ => 101
    }
    assert( x == 3 )
  }

  "Exercise 3.2" should "be correct" in {
    val l = List(1,2,3)

    assert( List.tail(l) == List(2,3) )
    assert( List.tail(Nil) == Nil )
  }

  "Exercise 3.3" should "be correct" in {
    val l = List(1,2,3)

    assert( List.setHead(l,4) == List(4,2,3))
    assert( List.setHead(Nil, 4) == Nil )
  }

  "Exercise 3.4" should "be correct" in {
    val l = List(1,2,3,4,5,6)

    assert( List.drop(l,0) == l )
    assert( List.drop(l,-1) == l )
    assert( List.drop(l,2) == List(3,4,5,6))
    assert( List.drop(l,4) == List(5,6))
    assert( List.drop(l,6) == Nil)
    assert( List.drop(l,7) == Nil)
  }
}
