package fpinscala.parallelism

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

/**
  * Created by Ardjan on 14/08/2016.
  */
class NonblockingSpec extends FlatSpec with Matchers {
  import Nonblocking.Par._


  "Exercise 7.11" should "choice true" in {
    val es = Executors.newSingleThreadExecutor

    val ct = choice(unit(true))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(ct) == 0)
  }

  "Exercise 7.11" should "choice false" in {
    val es = Executors.newSingleThreadExecutor

    val cf = choice(unit(false))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(cf) == 1)
  }

  "Exercise 7.11" should "choiceN 0" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceN(unit(0))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 0)
  }

  "Exercise 7.11" should "choiceN 1" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceN(unit(1))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 1)
  }

  "Exercise 7.11" should "choiceN 2" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceN(unit(2))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 2)
  }

  "Exercise 7.11" should "choiceViaChoiceN true" in {
    val es = Executors.newSingleThreadExecutor

    val ct = choiceViaChoiceN(unit(true))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(ct) == 0)
  }

  "Exercise 7.11" should "choiceViaChoiceN false" in {
    val es = Executors.newSingleThreadExecutor

    val cf = choiceViaChoiceN(unit(false))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(cf) == 1)
  }

  "Exercise 7.12 choiceMap" should "with a produce A" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cm = choiceMap(unit("a"))(m)

    assert(Nonblocking.Par.run(es)(cm) == "A")
  }

  "Exercise 7.12 choiceMap" should "with b produce B" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cm = choiceMap(unit("b"))(m)

    assert(Nonblocking.Par.run(es)(cm) == "B")
  }

  "Exercise 7.12 choiceMap" should "with c produce C" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cm = choiceMap(unit("c"))(m)

    assert(Nonblocking.Par.run(es)(cm) == "C")
  }

  "Exercise 7.13 chooser" should "with a produce A" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cn = chooser(unit("a"))(m)

    assert(Nonblocking.Par.run(es)(cn) == "A")
  }

  "Exercise 7.13 chooser" should "with b produce B" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cn = chooser(unit("b"))(m)

    assert(Nonblocking.Par.run(es)(cn) == "B")
  }

  "Exercise 7.13 chooser" should "with c produce C" in {
    val es = Executors.newSingleThreadExecutor

    val m = Map("a" -> unit("A"), "b" -> unit("B"), "c" -> unit("C"))

    val cn = chooser(unit("c"))(m)

    assert(Nonblocking.Par.run(es)(cn) == "C")
  }

  "Exercise 7.13 choiceViaChooser" should "with true produce 0" in {
    val es = Executors.newSingleThreadExecutor

    val ct = choiceViaChooser(unit(true))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(ct) == 1)
  }

  "Exercise 7.13 choiceViaChooser" should "with false produce 1" in {
    val es = Executors.newSingleThreadExecutor

    val cf = choiceViaChooser(unit(false))(unit(0), unit(1))

    assert(Nonblocking.Par.run(es)(cf) == 0)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 0 produce 0" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceNChooser(unit(0))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 0)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 1 produce 1" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceNChooser(unit(1))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 1)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 2 produce 2" in {
    val es = Executors.newSingleThreadExecutor

    val pl = List(unit(0), unit(1), unit(2))

    val cn = choiceNChooser(unit(2))(pl)

    assert(Nonblocking.Par.run(es)(cn) == 2)
  }



}
