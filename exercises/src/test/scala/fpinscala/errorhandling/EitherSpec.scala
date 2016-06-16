package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {
  "Exercise 4.6" should "map" in {
    val l = Left(1)
    val r = Right(2)

    assert(l.map(_.toString) == l)
    assert(r.map(_.toString) == Right("2"))
  }

  "Exercise 4.6" should "flatMap" in {
    val l = Left(1)
    val r = Right(2)

    assert(l.flatMap(x => Right(x)) == l)
    assert(r.flatMap(x => Right(x)) == r)
    assert(l.flatMap(_ => Left(3)) == l)
    assert(r.flatMap(_ => l) == l)
  }

  "Exercise 4.6" should "orElse" in {
    val l = Left(1)
    val r = Right(2)

    assert(l.orElse(r) == r)
    assert(l.orElse(Left(3)) == Left(3))
    assert(r.orElse(l) == r)
    assert(r.orElse(Right(3)) == r)
  }

  "Exercise 4.6" should "map2" in {
    val la = Left("la")
    val lb = Left("lb")
    val ra = Right(100)
    val rb = Right(1000.5)

    val f = (a: Int, b: Double) => (a + b).toString

    assert(f(100, 1000.5) == "1100.5")

    assert(la.map2(lb)(f) == la)
    assert(la.map2(rb)(f) == la)
    assert(ra.map2(rb)(f) == Right("1100.5"))
    assert(ra.map2(lb)(f) == lb)
    assert(ra.map2(rb)((_, _) => lb) == Right(lb))
  }

  "Exercise 4.7" should "sequence" in {
    val l = List(Left(0), Left(1))
    val r = List(Right(0), Right(1))
    val lr = List(Left(0), Right(1))
    val rl = List(Right(0), Left(1))

    assert(Either.sequence(l) == Left(0))
    assert(Either.sequence(r) == Right(List(0, 1)))
    assert(Either.sequence(lr) == Left(0))
    assert(Either.sequence(rl) == Left(1))
  }

  "Exercise 4.7" should "traverse" in {
    val l = List(Left(0), Left(1))
    val r = List(Right(0), Right(1))
    val lr = List(Left(0), Right(1))
    val rl = List(Right(0), Left(1))

    assert(Either.traverse(l)(x => x.map(_.toString)) == Left(0))
    assert(Either.traverse(r)(x => x.map(_.toString)) == Right(List("0", "1")))
    assert(Either.traverse(lr)(x => x.map(_.toString)) == Left(0))
    assert(Either.traverse(rl)(x => x.map(_.toString)) == Left(1))
  }
}
