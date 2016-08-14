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
}

