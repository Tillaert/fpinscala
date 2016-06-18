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

}
