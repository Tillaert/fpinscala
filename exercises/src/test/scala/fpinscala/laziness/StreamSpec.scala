package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers{
  "Exercise 5.1" should "toList 1,2,3,4" in {
    val s = Stream(1,2,3,4)

    assert( s.toList == List(1,2,3,4))
  }

  "Exercise 5.1" should "toList Empty" in {
    val s = Empty

    assert( s.toList == Nil )
  }

  "Exercise 5.2" should "take(n)" in {
    assert( Stream(1,2,3,4).take(0) == Empty)
    assert( Stream(1,2,3,4).take(2).toList == List(1,2))
    assert( Stream(1,2,3,4).take(4).toList == List(1,2,3,4) )
    assert( Empty.take(1) == Empty )
    assert( Stream(1,2,3,4).take(5).toList == List(1,2,3,4))
  }

  "Exercise 5.2" should "drop(n)" in {
    assert( Stream(1,2,3,4).drop(0).toList == List(1,2,3,4))
    assert( Stream(1,2,3,4).drop(2).toList == List(3,4))
    assert( Stream(1,2,3,4).drop(4) == Empty )
    assert( Empty.drop(1) == Empty )
    assert( Stream(1,2,3,4).drop(5) == Empty)
  }

  "Exercise 5.3" should "takeWhile" in {
    assert( Stream(1,1,2,2).takeWhile( _ == 1 ).toList == List(1,1))
    assert( Empty.takeWhile( _ => true ) == Empty )
    assert( Stream(2,2,2,2).takeWhile( _ == 1 ).toList == Nil )
    assert( Stream(1,1,1,1).takeWhile( _ == 1 ).toList == List(1,1,1,1))
  }
}
