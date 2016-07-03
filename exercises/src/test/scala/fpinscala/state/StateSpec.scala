package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {
  "Exercise 6.1" should "nonNegativeInt" in {
    val r = RNG.Mock(42)
    assert(RNG.nonNegativeInt(r) ==(42, r))

    val r2 = RNG.Mock(-42)
    assert(RNG.nonNegativeInt(r2) ==(42, r2))

    val r3 = RNG.Mock(0)
    assert(RNG.nonNegativeInt(r3) ==(0, r3))

    val r4 = RNG.Mock(Int.MinValue)
    assert(RNG.nonNegativeInt(r4) ==(0, r4))
  }

  "Exercise 6.1" should "doulbe" in {
    val r = RNG.Mock(1234)
    assert(RNG.double(r) == (1234 / (Int.MaxValue.toDouble + 1), r))
  }
}
