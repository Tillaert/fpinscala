package fpinscala.testing

import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {
  def sum(l: List[Int]): Int = l.sum

  "Exercise 8.1 sum reversed" should "be equal to the regular sum" in {
    val intList = Gen.listOfN(10, Gen.choose(0, 100))

    val prop = Prop.forAll(intList)(ns => sum(ns.reverse) == sum(ns))

    Prop.run(prop)
  }

  "Exercise 8.1 sum of same values" should "be equal to the length of the list times that value" in {
    val value = Gen.unit[Int](10)

    val prop = Prop.forAll(value)(v => sum(List.fill(100)(v)) == 100 * v)

    Prop.run(prop)
  }

  def max(l: List[Int]): Int = l.max

  "Exercise 8.2 max of a list" should "be greater than any value of that list" in {
    val intList = Gen.listOfN(10, Gen.choose(0, 100))

    val prop = Prop.forAll(intList)(ns => max(ns) == ns.max)

    Prop.run(prop)
  }

  "Exercise 8.2 max of a list of one item" should "be that item" in {
    val value = Gen.unit[Int](10)

    val prop = Prop.forAll(value)(v => max(List(v)) == v)

    Prop.run(prop)
  }

  "Exercise 8.2 max of a list of items with the same value" should "be that item" in {
    val value = Gen.unit(10)

    val prop = Prop.forAll(value)(v => max(List.fill(100)(v)) == v)

    Prop.run(prop)
  }

  "Exercise 8.3 &&" should "combine two props" in {
    val value = Gen.choose(1, 100)

    val prop = Prop.forAll(value)(v => v >= 1) &&
      Prop.forAll(value)(v => v < 100)

    Prop.run(prop)
  }

  "Exercise 8.4 choose " should "generate a value between start and stop" in {
    val value = Gen.choose(5, 10)

    val prop = Prop.forAll(value)(v => v >= 5 && v < 10)

    Prop.run(prop)
  }

  "Exercise 8.5 Gen.unit" should "compile" in {
    val value = Gen.unit(10)
  }

  "Exercise 8.5 Gen.boolean" should "compile" in {
    val value = Gen.boolean
  }

  "Exercise 8.5 Gen.listOfN" should "compile" in {
    val value = Gen.listOfN(10, Gen.choose(0, 10))
  }

  "Exercise 8.5 Gen.flatmap" should "compile" in {
    val value = Gen.unit(10).flatMap((i: Int) => Gen.unit(i + 10))
  }

  "Exercise 8.13 maxProp" should "pass" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf(smallInt)) {
      ns => {
        val max = ns.max
        !ns.exists(_ > max)
      }
    }

    Prop.run(maxProp)
  }
}

