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
    assert(Empty.filter(_ => true) == Empty)
    assert(Stream(1, 2, 3, 4).filter(_ => false).toList == Nil)
  }


  "Exercise 5.7" should "append" in {
    assert(Stream(1, 2).append(Stream(3, 4)).toList == List(1, 2, 3, 4))
    assert(Stream().append(Stream(3, 4)).toList == List(3, 4))
    assert(Stream(1, 2).append(Empty).toList == List(1, 2))
    assert(Empty.append(Empty).toList == Nil)
  }


  "Exercise 5.7" should "flatMap" in {
    assert(Stream(1, 2).flatMap(x => Stream(x, x)).toList == List(1, 1, 2, 2))
    assert(Empty.flatMap(_ => Stream(1)).toList == Nil)
  }

  "Exercise 5.8" should "constant" in {
    assert(Stream.constant(5).take(4).toList == List(5, 5, 5, 5))
    assert(Stream.constant(1).map(_ * 10).take(10).toList == List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
  }

  "Exercise 5.9" should "from" in {
    assert(Stream.from(10).take(2).toList == List(10, 11))
    assert(Stream.from(100).take(4).toList == List(100, 101, 102, 103))
    assert(Stream.from(-10).take(3).toList == List(-10, -9, -8))
  }

  "Exercise 5.10" should "fibs" in {
    assert(Stream.fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  "Exercise 5.11" should "unfold" in {
    assert(Stream.unfold(7)( x => Some(x,x+1)).take(4).toList == List(7,8,9,10))
  }

  "Exercise 5.12" should "fibsViaUnfold" in {
    assert(Stream.fibsViaUnfold.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  "Exercise 5.12" should "fromViaUnfold" in {
    assert(Stream.fromViaUnfold(10).take(2).toList == List(10, 11))
    assert(Stream.fromViaUnfold(100).take(4).toList == List(100, 101, 102, 103))
    assert(Stream.fromViaUnfold(-10).take(3).toList == List(-10, -9, -8))
  }

  "Exercise 5.12" should "constantViaUnfold" in {
    assert(Stream.constantViaUnfold(5).take(4).toList == List(5, 5, 5, 5))
    assert(Stream.constantViaUnfold(1).map(_ * 10).take(10).toList == List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))

  }

  "Exercise 5.12" should "onesViaUnfold" in {
    assert(Stream.onesViaUnfold.take(3).toList == List(1,1,1))
  }

}
