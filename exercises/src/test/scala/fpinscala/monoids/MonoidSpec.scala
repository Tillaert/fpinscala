package fpinscala.monoids


import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {
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

    Monoid.concatenate(words, Monoid.stringMonoid) should be ("HicEstIndex")
  }

  "foldMap" should "map and then fold using a monoid" in {
    val numbers = List(123,456,789)

    Monoid.foldMap(numbers,Monoid.stringMonoid)(_.toString) should be ("123456789")
  }

}
