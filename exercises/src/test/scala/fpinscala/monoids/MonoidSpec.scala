package fpinscala.monoids


import org.scalatest.{FlatSpec, Matchers}

class MonoidSpec extends FlatSpec with Matchers {
  // These are simple tests as we implement generated law tests later
  "intAddition" should "obey monoid laws" in {
    def m = Monoid.intAddition

    m.op(1, 2) should be(3)
    m.op(1, m.op(2, 3)) should be(m.op(m.op(1, 2), 3))
    m.op(1, m.zero) should be(1)
  }

  "intMultiplication" should "obey monoid laws" in {
    def m = Monoid.intMultiplication

    m.op(2, 3) should be(6)
    m.op(2, m.op(3, 4)) should be(m.op(m.op(2, 3), 4))
    m.op(2, m.zero) should be(2)
  }

  "booleanOr" should "obey monoid laws" in {
    def m = Monoid.booleanOr

    m.op(true, true) should be(true)
    m.op(true, false) should be(true)
    m.op(false, true) should be(true)
    m.op(false, false) should be(false)
    m.op(false, m.op(true, false)) should be(m.op(m.op(false, true), false))
    m.op(true, m.zero) should be(true)
    m.op(false, m.zero) should be(false)
  }

  "booleanAnd" should "obey monoid laws" in {
    def m = Monoid.booleanAnd

    m.op(true, true) should be(true)
    m.op(true, false) should be(false)
    m.op(false, true) should be(false)
    m.op(false, false) should be(false)
    m.op(true, m.op(false, true)) should be(m.op(m.op(true, false), true))
    m.op(true, m.zero) should be(true)
    m.op(false, m.zero) should be(false)
  }

  "optionMonoid" should "obey monoid laws" in {
    val s1 = Some(1)
    val s2 = Some(2)
    val s3 = Some(3)

    val m = Monoid.optionMonoid[Int]

    m.op(m.op(s1, s2), s3) should be(m.op(s1, m.op(s2, s3)))
    m.op(s1, m.zero) should be(s1)
    m.op(m.zero, s2) should be(s2)
  }
}
