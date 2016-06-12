package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ardjan on 12/06/2016.
  */
class TreeSpec extends FlatSpec with Matchers {
  "Exercise 3.25" should "size" in {
    val l = Leaf(1)
    val b = Branch(Leaf(1), Leaf(2))
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.size(l) == 1)
    assert(Tree.size(b) == 3)
    assert(Tree.size(t) == 5)
  }

  "Exercise 3.26" should "maximum" in {
    val t = Branch(Branch(Leaf(1), Leaf(-100)), Leaf(1000))

    assert(Tree.maximum(t) == 1000)
  }

  "Exercise 3.27" should "depth" in {
    val l = Leaf(1)
    val b = Branch(Leaf(1), Leaf(1))
    val t = Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1))

    assert(Tree.depth(l) == 0)
    assert(Tree.depth(b) == 1)
    assert(Tree.depth(t) == 3)
  }

  "Exercise 3.28" should "map" in {
    val l = Leaf(1)
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.map(l)(_.toString) == Leaf("1"))
    assert(Tree.map(b)(_.toString) == Branch(Branch(Leaf("1"), Leaf("2")),Leaf("3")))
  }

  "Exercise 3.29" should "sizeFold" in {
    val l = Leaf(1)
    val b = Branch(Leaf(1), Leaf(2))
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.sizeFold(l) == 1)
    assert(Tree.sizeFold(b) == 3)
    assert(Tree.sizeFold(t) == 5)
  }

  "Exercise 3.29" should "maximumFold" in {
    val t = Branch(Branch(Leaf(1), Leaf(-100)), Leaf(1000))

    assert(Tree.maximumFold(t) == 1000)
  }

  "Exercise 3.29" should "depthFold" in {
    val l = Leaf(1)
    val b = Branch(Leaf(1), Leaf(1))
    val t = Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1))

    assert(Tree.depthFold(l) == 0)
    assert(Tree.depthFold(b) == 1)
    assert(Tree.depthFold(t) == 3)
  }

  "Exercise 3.29" should "map" in {
    val l = Leaf(1)
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(Tree.mapFold(l)(_.toString) == Leaf("1"))
    assert(Tree.mapFold(b)(_.toString) == Branch(Branch(Leaf("1"), Leaf("2")),Leaf("3")))
  }


}
