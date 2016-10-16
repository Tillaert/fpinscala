package fpinscala.monoids


import fpinscala.testing.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {
  // These are simple tests as we implement generated law tests later
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
    def f1(i: Int) = i + 2
    def f2(i: Int) = i + 3
    def f3(i: Int) = i + 5

    val m = Monoid.endoMonoid[Int]

    m.op(f1, f2)(10) should be(15)
    m.op(m.op(f1, f2), f3)(100) should be(m.op(f1, m.op(f2, f3))(100))
    m.op(f1, m.zero)(10) should be(f1(10))
  }
}
