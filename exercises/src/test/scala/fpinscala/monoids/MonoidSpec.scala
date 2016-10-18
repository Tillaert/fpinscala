package fpinscala.monoids


import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking.Par
import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {

  import Monoid._

  "intAddition" should "obey monoid laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.intAddition, Gen.integer))
  }

  "intMultiplication" should "obey monoid laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.intMultiplication, Gen.integer))
  }

  "booleanOr" should "obey monoid laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.booleanOr, Gen.boolean))
  }

  "booleanAnd" should "obey monoid laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.booleanAnd, Gen.boolean))
  }

  "optionMonoid" should "obey monoid laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.optionMonoid[Int], Gen.integer.map(Some(_))) &&
      Monoid.monoidLaws(Monoid.optionMonoid[Boolean], Gen.boolean.map(_ => None)))
  }

  "endoMonoid" should "obey monoid laws" in {
    // We do not have a generator for Gen[A => A] although we could implement this as a
    // generator that creates a LUT?
    def f1(i: Int) = i + 2
    def f2(i: Int) = i + 3
    def f3(i: Int) = i + 5

    val m = Monoid.endoMonoid[Int]

    m.op(f1, f2)(10) should be(15)
    m.op(m.op(f1, f2), f3)(100) should be(m.op(f1, m.op(f2, f3))(100))
    m.op(f1, m.zero)(10) should be(f1(10))
  }

  "concatenate" should "concatenate a list of strings" in {
    val words = List("Hic", "Est", "Index")

    Monoid.concatenate(words, Monoid.stringMonoid) should be("HicEstIndex")
  }

  "foldMap" should "map and then fold using a monoid" in {
    val numbers = List(123, 456, 789)

    Monoid.foldMap(numbers, Monoid.stringMonoid)(_.toString) should be("123456789")
  }

  "foldRight" should "be implemented via foldMap" in {
    val l = List(1, 2, 3, 4)

    Monoid.foldRight(l)(List[Int]())(_ :: _) should be(List(4, 3, 2, 1))
  }

  "foldLeft" should "be implemented via foldMap" in {
    val l = List(1, 2, 3, 4)

    Monoid.foldLeft(l)(List[Int]())((b, a) => a :: b) should be(List(1, 2, 3, 4))
  }

  "foldMapV" should "map and then fold using a monoid" in {
    val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    Monoid.foldMap(numbers, Monoid.stringMonoid)(_.toString) should be("123456789")
  }

  "parFoldMap" should "map items in parallel" in {
    val numbers = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9)

    val es = Executors.newSingleThreadExecutor

    Par.run(es)(Monoid.parFoldMap(numbers, Monoid.stringMonoid)(_.toString)) should be("123456789")
  }

  "numbers" should "be ordered" in {
    val numbers = IndexedSeq(-100, -10, 3, 1000000, 444444444)

    Monoid.ordered(numbers) should be(true)
  }

  "numbers" should "not be ordered" in {
    val numbers = IndexedSeq(1, 2, 3, 5, 4, 6, 7)

    Monoid.ordered(numbers) should not be true
  }

  "same numbers" should "be ordered" in {
    val numbers = IndexedSeq(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)

    Monoid.ordered(numbers) should be(true)
  }


  "wcMonoid" should "handle two parts" in {
    wcMonoid.op(Part("fee", 2, "fi"), Part("fo", 3, "fum")) should be(Part("fee", 6, "fum"))
    wcMonoid.op(Part("fee", 2, "fi"), Stub("fo")) should be(Part("fee", 2, "fifo"))
    wcMonoid.op(Stub("fo"), Part("fee", 2, "fi")) should be(Part("fofee", 2, "fi"))
    wcMonoid.op(Stub("fo"), Stub("fi")) should be(Stub("fofi"))
    wcMonoid.op(Part("fee", 2, ""), Part("fi", 3, "fo")) should be(Part("fee", 6, "fo"))
    wcMonoid.op(Part("fee", 3, "fi"), Part("", 3, "fo")) should be(Part("fee", 7, "fo"))
    wcMonoid.op(Part("fee", 3, ""), Part("", 3, "fum")) should be(Part("fee", 6, "fum"))
  }

  "wcMonoid" should "obey monoid laws" in {
    // We could use a better Gen[WC]
    val g = Gen.boolean.flatMap(
      b => if (b)
        for {
          c <- Gen.choose(0, 10)
          s <- Gen.stringN(c) map (_.filter(!_.isWhitespace))
        } yield Stub(s)
      else
        for {
          lc <- Gen.choose(0, 4)
          l <- Gen.stringN(lc) map (_.filter(!_.isWhitespace))
          c <- Gen.integer
          rc <- Gen.choose(0, 4)
          r <- Gen.stringN(rc) map (_.filter(!_.isWhitespace))
        } yield Part(l, c, r)
    )

    Prop.run(Monoid.monoidLaws(Monoid.wcMonoid, g))
  }

  "count" should """parse "lorem ipsum do" as 3""" in {
    val str = "lorem ipsum do"

    count(str) should be(3)
  }

  "count" should """parse "lor sit amet" as 3""" in {
    val str = "lor sit amet"

    count(str) should be(3)
  }

  "count" should "count 5 words" in {
    val str = "lorem ipsum dolor ist amet, "

    count(str) should be(5)
  }

  "count" should "count correctly" in {
    count("") should be(0)
    count(" ") should be(0)
    count("f") should be(1)
  }
}
