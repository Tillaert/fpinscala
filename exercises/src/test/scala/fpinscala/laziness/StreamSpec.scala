package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {
  "Exercise 5.1" should "toList 1,2,3,4" in {
    val s = Stream(1, 2, 3, 4)

    assert(s.toList == List(1, 2, 3, 4))
  }

  "Exercise 5.1" should "toList Empty" in {
    val s = Empty

    assert(s.toList == Nil)
  }

  "Exercise 5.2" should "take(n)" in {
    assert(Stream(1, 2, 3, 4).take(0) == Empty)
    assert(Stream(1, 2, 3, 4).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4).take(4).toList == List(1, 2, 3, 4))
    assert(Empty.take(1) == Empty)
    assert(Stream(1, 2, 3, 4).take(5).toList == List(1, 2, 3, 4))
  }

  "Exercise 5.2" should "drop(n)" in {
    assert(Stream(1, 2, 3, 4).drop(0).toList == List(1, 2, 3, 4))
    assert(Stream(1, 2, 3, 4).drop(2).toList == List(3, 4))
    assert(Stream(1, 2, 3, 4).drop(4) == Empty)
    assert(Empty.drop(1) == Empty)
    assert(Stream(1, 2, 3, 4).drop(5) == Empty)
  }

  "Exercise 5.3" should "takeWhile" in {
    assert(Stream(1, 1, 2, 2).takeWhile(_ == 1).toList == List(1, 1))
    assert(Empty.takeWhile(_ => true) == Empty)
    assert(Stream(2, 2, 2, 2).takeWhile(_ == 1).toList == Nil)
    assert(Stream(1, 1, 1, 1).takeWhile(_ == 1).toList == List(1, 1, 1, 1))
  }

  "Exercise 5.4" should "forAll" in {
    assert(!Stream(1, 1, 1, 2, 1).forAll(_ == 1))
    assert(Stream(1, 1, 1, 1, 1).forAll(_ == 1))
    assert(Empty.forAll(_ => false))
  }

  "Exercise 5.5" should "takeWhileViaFoldRight" in {
    assert(Stream(1, 1, 2, 2).takeWhileViaFoldRight(_ == 1).toList == List(1, 1))
    assert(Empty.takeWhileViaFoldRight(_ => true) == Empty)
    assert(Stream(2, 2, 2, 2).takeWhileViaFoldRight(_ == 1).toList == Nil)
    assert(Stream(1, 1, 1, 1).takeWhileViaFoldRight(_ == 1).toList == List(1, 1, 1, 1))
  }

  "Exercise 5.6" should "headOption" in {
    assert(Stream(1, 2, 3, 4).headOption contains 1)
    assert(Stream(2).headOption contains 2)
    assert(Empty.headOption.isEmpty)
  }

  "Exercise 5.7" should "map" in {
    assert(Stream(1, 2, 3, 4).map(_.toString).toList == List("1", "2", "3", "4"))
    assert(Empty.map(_.toString) == Empty)
  }

  "Exercise 5.7" should "filter" in {
    assert(Stream(1, 2, 3, 4).filter(_ < 3).toList == List(1, 2))
    assert(Empty.filter( _ => true) == Empty)
    assert(Stream(1, 2, 3, 4).filter( _ => false).toList == Nil)
  }


  "Exercise 5.7" should "append" in {
    assert(Stream(1, 2).append(Stream(3, 4)).toList == List(1, 2, 3, 4))
    assert(Stream().append( Stream(3, 4)).toList == List(3,4))
    assert(Stream(1,2).append(Empty).toList == List(1,2))
    assert(Empty.append(Empty).toList == Nil)
  }


  "Exercise 5.7" should "flatMap" in {
    assert(Stream(1,2).flatMap( x => Stream(x,x)).toList == List(1,1,2,2))
    assert(Empty.flatMap( _ => Stream(1)).toList == Nil )
  }
}