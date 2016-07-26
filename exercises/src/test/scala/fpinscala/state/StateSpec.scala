package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {
  "Exercise 6.1" should "nonNegativeInt" in {
    val r0 = RNG.Simple(0)
    val r = RNG.Mock(42, r0)
    assert(RNG.nonNegativeInt(r) == (42, r0))

    val r2 = RNG.Mock(-42, r0)
    assert(RNG.nonNegativeInt(r2) == (42, r0))

    val r3 = RNG.Mock(0, r0)
    assert(RNG.nonNegativeInt(r3) == (0, r0))

    val r4 = RNG.Mock(Int.MinValue, r0)
    assert(RNG.nonNegativeInt(r4) == (0, r0))
  }

  "Exercise 6.2" should "double" in {
    val r0 = RNG.Simple(0)
    val r = RNG.Mock(1234)
    assert(RNG.double(r) == (1234 / (Int.MaxValue.toDouble + 1), r0))
  }

  "Exercise 6.3" should "intDouble" in {
    val r3 = RNG.Mock(3456)
    val r2 = RNG.Mock(2345, r3)
    val r1 = RNG.Mock(1234, r2)
    val d2 = 2345 / (Int.MaxValue.toDouble + 1)

    assert(r2.nextInt == (2345, r3))

    assert(RNG.intDouble(r1) == ((1234, d2), r3))
  }

  "Exercise 6.3" should "doubleInt" in {
    val r3 = RNG.Mock(3456)
    val r2 = RNG.Mock(2345, r3)
    val r1 = RNG.Mock(1234, r2)
    val d1 = 1234 / (Int.MaxValue.toDouble + 1)

    assert(RNG.doubleInt(r1) == ((d1, 2345), r3))
  }

  "Exercise 6.3" should "double3" in {
    val r4 = RNG.Mock(4567)
    val r3 = RNG.Mock(3456, r4)
    val r2 = RNG.Mock(2345, r3)
    val r1 = RNG.Mock(1234, r2)
    val d1 = 1234 / (Int.MaxValue.toDouble + 1)
    val d2 = 2345 / (Int.MaxValue.toDouble + 1)
    val d3 = 3456 / (Int.MaxValue.toDouble + 1)

    assert(RNG.double3(r1) == ((d1, d2, d3), r4))
  }

  "Exercise 6.4" should "ints" in {
    val r4 = RNG.Mock(4567)
    val r3 = RNG.Mock(3456, r4)
    val r2 = RNG.Mock(2345, r3)
    val r1 = RNG.Mock(1234, r2)

    assert(RNG.ints(3)(r1) == (List(1234, 2345, 3456), r4))
  }

  "Exercise 6.5" should "double via map" in {
    val r2 = RNG.Mock(2345)
    val r = RNG.Mock(1234, r2)
    val d = 1234 / (Int.MaxValue.toDouble + 1)

    assert(RNG.doubleViaMap(r) == (d, r2))
  }

  "Exercise 6.6" should "map2" in {
    val ra = RNG.unit(1234)
    val rb = RNG.unit(2345)

    val r = RNG.Mock(666)

    assert(RNG.map2(ra, rb)(_ + _)(r) == (1234 + 2345, r))
  }

  "Exercise 6.7" should "sequence" in {
    val ints = List(RNG.unit(1), RNG.unit(2), RNG.unit(3))

    val r = RNG.Simple(0)

    assert(RNG.sequence(ints)(r)._1 == List(1, 2, 3))
  }

  "Exercise 6.8" should "flatMap" in {
    val r = RNG.unit(1234)
    val rng = RNG.Simple(0)

    assert(RNG.flatMap(r) { i: Int => RNG.unit(i + 1) }(rng)._1 == 1235)
  }

  "Exercise 6.9" should "map via flatMap" in {
    val r = RNG.unit(1234)

    val rng = RNG.Mock(666)

    assert(RNG.mapViaFlatMap(r)(_.toString)(rng)._1 == "1234")
  }

  "Exercise 6.9" should "map2 via flatMap" in {
    val ra = RNG.unit(1234)
    val rb = RNG.unit(2345)

    val r = RNG.Mock(666)

    assert(RNG.map2ViaFlatMap(ra, rb)(_ + _)(r)._1 == 1234 + 2345)
  }

  "Exercise 6.10" should "state unit" in {
    val s = State.unit[Int, Int](123)

    assert(s.run(1)._1 == 123)
  }

  "Exercise 6.10" should "state map" in {
    val s = State.unit[Int, Int](123)

    assert(s.map(_.toString).run(1)._1 == "123")
  }

  "Exercise 6.10" should "state map2" in {
    val ra = State.unit[Int, Int](1234)
    val rb = State.unit[Int, Int](2345)

    assert(ra.map2(rb)(_ + _).run(2)._1 == 1234 + 2345)
  }

  "Exercise 6.10" should "state flatMap" in {
    assert(State.unit[Int, Int](1234).flatMap { i: Int => State.unit(i + 1) }.run(1)._1 == 1235)
  }

  "Exercise 6.10" should "state sequence" in {
    val ints = List(State.unit[Int, Int](1), State.unit[Int, Int](2), State.unit[Int, Int](3))

    assert(State.sequence(ints).run(1)._1 == List(1, 2, 3))
  }

  "Exercise 6.11" should "unlock a candy" in {
    assert(CandyMachine.simulateMachine(List(Coin, Turn)).run(Machine(locked = true, 5, 0))._1 == (1, 4))
  }

  "Exercise 6.11" should "turning the knob on an unlocked machine" in {
    assert(CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = false, 5, 0))._1 == (1, 4))
  }

  "Exercise 6.11" should "turning the knob on a locked machine does nothing" in {
    assert(CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = true, 5, 0))._1 == (0, 5))
  }

  "Exercise 6.11" should "inserting a coin in an unlocked machine does nothing" in {
    assert(CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = false, 5, 0))._1 == (0, 5))
  }

  "Exercise 6.11" should "a machine that's out of candy ignores coins" in {
    assert(CandyMachine.simulateMachine(List(Coin)).run(Machine(locked = true, 0, 0))._1 == (0, 0))
  }

  "Exercise 6.11" should "a machine that's out of candyh ignores turns" in {
    assert(CandyMachine.simulateMachine(List(Turn)).run(Machine(locked = false, 0, 0))._1 == (0, 0))
  }

  "Exercise 6.11" should "simulateState" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val machine = Machine(locked = true, 5, 10)

    assert(CandyMachine.simulateMachine(inputs).run(machine)._1 == (14, 1))
  }
}
