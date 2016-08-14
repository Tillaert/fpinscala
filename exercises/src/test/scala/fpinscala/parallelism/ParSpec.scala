package fpinscala.parallelism

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._

/**
  * Created by Ardjan on 13/06/2016.
  */
class ParSpec extends FlatSpec with Matchers {
  "Exercise 7.1" should "be specified" in {
    /*
    Par.map2[A,B,C](l: Par[A], r: Par[B])(f: (A,B) => C): Par[C]

     */
  }

  "Exercise 7.4" should "unit" in {
    val es = Executors.newSingleThreadExecutor

    assert(Par.run(es)(Par.unit(10)).get == 10)
  }

  "Exercise 7.4" should "lazyunit" in {
    val es = Executors.newSingleThreadExecutor

    assert(Par.run(es)(Par.lazyUnit(10)).get == 10)
  }

  "Exercise 7.4" should "asyncF" in {
    val es = Executors.newSingleThreadExecutor

    assert(Par.run(es)(Par.asyncF((i: Int) => i.toString)(10)).get == "10")
  }

  "Exercise 7.4" should "fork" in {
    val es = Executors.newSingleThreadExecutor

    assert(Par.run(es)(Par.fork(Par.unit(10))).get == 10)
  }

  "Exercise 7.4" should "map2" in {
    val es = Executors.newSingleThreadExecutor

    assert(Par.run(es)(Par.map2(Par.unit(10), Par.unit(1000))(_ + _)).get == 1010)
  }

  "Exercise 7.5" should "sequence" in {
    val es = Executors.newSingleThreadExecutor

    val l = List(Par.unit(100), Par.lazyUnit(10), Par.unit(1))

    assert(Par.run(es)(Par.sequence(l)).get == List(100, 10, 1))
  }

  "Exercise 7.6" should "perFilter" in {
    // Since this implementation does not abide to the laws, it will block indefinately on a single threaded executor.
    val es = Executors.newCachedThreadPool

    val l = List(0, 1, 2, 3, 4, 5)

    def f(i: Int) = i % 2 == 0

    assert(Par.run(es)(Par.parFilter(l)(f)).get == List(0, 2, 4))
  }

  "Exercise 7.11" should "choice true" in {
    val es = Executors.newCachedThreadPool

    val ct = Par.choice(Par.unit(true))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(ct).get == 0)
  }

  "Exercise 7.11" should "choice false" in {
    val es = Executors.newCachedThreadPool

    val cf = Par.choice(Par.unit(false))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(cf).get == 1)
  }

  "Exercise 7.11" should "choiceN 0" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceN(Par.unit(0))(pl)

    assert(Par.run(es)(cn).get == 0)
  }

  "Exercise 7.11" should "choiceN 1" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceN(Par.unit(1))(pl)

    assert(Par.run(es)(cn).get == 1)
  }

  "Exercise 7.11" should "choiceN 2" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceN(Par.unit(2))(pl)

    assert(Par.run(es)(cn).get == 2)
  }

  "Exercise 7.11" should "choiceViaChoiceN true" in {
    val es = Executors.newCachedThreadPool

    val ct = Par.choiceViaChoiceN(Par.unit(true))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(ct).get == 0)
  }

  "Exercise 7.11" should "choiceViaChoiceN false" in {
    val es = Executors.newCachedThreadPool

    val cf = Par.choiceViaChoiceN(Par.unit(false))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(cf).get == 1)
  }

  "Exercise 7.12 choiceMap" should "with a produce A" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cm = Par.choiceMap(Par.unit("a"))(m)

    assert(Par.run(es)(cm).get == "A")
  }

  "Exercise 7.12 choiceMap" should "with b produce B" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cm = Par.choiceMap(Par.unit("b"))(m)

    assert(Par.run(es)(cm).get == "B")
  }

  "Exercise 7.12 choiceMap" should "with c produce C" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cm = Par.choiceMap(Par.unit("c"))(m)

    assert(Par.run(es)(cm).get == "C")
  }

  "Exercise 7.12 choiceViaChoiceMap" should "with true produce 0" in {
    val es = Executors.newCachedThreadPool

    val ct = Par.choiceViaChoiceMap(Par.unit(true))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(ct).get == 0)
  }

  "Exercise 7.12 choiceViaChoiceMap" should "with false produce" in {
    val es = Executors.newCachedThreadPool

    val cf = Par.choiceViaChoiceMap(Par.unit(false))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(cf).get == 1)
  }

  "Exercise 7.12 choiceNViaChoiceMap" should "with 0 produce 0" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChoiceMap(Par.unit(0))(pl)

    assert(Par.run(es)(cn).get == 0)
  }

  "Exercise 7.12 choiceNViaChoiceMap" should "with 1 produce 1" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChoiceMap(Par.unit(1))(pl)

    assert(Par.run(es)(cn).get == 1)
  }

  "Exercise 7.12 choiceNViaChoiceMap" should "with 2 produce 2" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChoiceMap(Par.unit(2))(pl)

    assert(Par.run(es)(cn).get == 2)
  }

  "Exercise 7.13 chooser" should "with a produce A" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cn = Par.chooser(Par.unit("a"))(m)

    assert(Par.run(es)(cn).get == "A")
  }

  "Exercise 7.13 chooser" should "with b produce B" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cn = Par.chooser(Par.unit("b"))(m)

    assert(Par.run(es)(cn).get == "B")
  }

  "Exercise 7.13 chooser" should "with c produce C" in {
    val es = Executors.newCachedThreadPool

    val m = Map("a" -> Par.unit("A"), "b" -> Par.unit("B"), "c" -> Par.unit("C"))

    val cn = Par.chooser(Par.unit("c"))(m)

    assert(Par.run(es)(cn).get == "C")
  }

  "Exercise 7.13 choiceViaChooser" should "with true produce 0" in {
    val es = Executors.newCachedThreadPool

    val ct = Par.choiceViaChooser(Par.unit(true))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(ct).get == 0)
  }

  "Exercise 7.13 choiceViaChooser" should "with false produce" in {
    val es = Executors.newCachedThreadPool

    val cf = Par.choiceViaChooser(Par.unit(false))(Par.unit(0), Par.unit(1))

    assert(Par.run(es)(cf).get == 1)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 0 produce 0" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChooser(Par.unit(0))(pl)

    assert(Par.run(es)(cn).get == 0)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 1 produce 1" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChooser(Par.unit(1))(pl)

    assert(Par.run(es)(cn).get == 1)
  }

  "Exercise 7.13 choiceNViaChooser" should "with 2 produce 2" in {
    val es = Executors.newCachedThreadPool

    val pl = List(Par.unit(0), Par.unit(1), Par.unit(2))

    val cn = Par.choiceNViaChooser(Par.unit(2))(pl)

    assert(Par.run(es)(cn).get == 2)
  }
  

}

