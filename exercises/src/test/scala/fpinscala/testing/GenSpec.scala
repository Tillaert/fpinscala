package fpinscala.testing

import org.scalatest.{FlatSpec, Matchers}

class GenSpec extends FlatSpec with Matchers {
  def sum(l: List[Int]): Int = l.sum

  "Exercise 8.1 sum reversed" should "be equal to the regular sum" in {
    val intList = Gen.listOf(Gen.choose(0, 100))

    val prop = Prop.forAll(intList)(ns => sum(ns.reverse) == sum(ns))

    prop.check
  }

  "Exercise 8.1 sum of same values" should "be equal to the length of the list times that value" in {
    val value = Gen.unit[Int](10)

    val prop = Prop.forAll(value)( v => sum(List.fill(100)(v)) == 100*v)

    prop.check
  }

  def max(l: List[Int]): Int = l.max
  "Exercise 8.2 max of a list" should "be greater than any value of that list" in {
    val intList = Gen.listOf(Gen.choose(0,100))

    val prop = Prop.forAll(intList)(ns => max(ns) == ns.max)

    prop.check
  }

  "Exercise 8.2 max of a list of one item" should "be that item" in {
    val value = Gen.unit[Int](10)

    val prop = Prop.forAll(value)( v => max(List(v))  == v )

    prop.check
  }

  "Exercise 8.2 max of a list of items with the same value" should "be that item" in {
    val value = Gen.unit(10)

    val prop = Prop.forAll(value)( v => max(List.fill(100)(v)) == v )

    prop.check
  }
}

