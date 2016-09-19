package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG
import fpinscala.laziness.Stream
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified: Boolean = true
}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def &&(that: Prop): Prop = new Prop(
    (ms, tc, rng) => Prop.this.run(ms, tc, rng) match {
      case Passed => that.run(ms, tc, rng)
      case f => f
    }
  )

  def ||(that: Prop): Prop = new Prop(
    (ms, tc, rng) =>
      Prop.this.run(ms, tc, rng) match {
        case f: Falsified => that.run(ms, tc, rng)
        case x => x
      }
  )

  def tag(tag: String): Prop = new Prop(
    (ms, tc, rng) =>
      Prop.this.run(ms, tc, rng) match {
        case f: Falsified => Falsified(s"${tag} ${f.failure}", f.successes)
        case x => x
      }
  )
}

object Prop {
  type MaxSize = Int
  type TestCases = Int

  type FailedCase = String
  type SuccessCount = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n spassed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
    }
}


object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]) = SGen( n => g.listOfN(n max 1))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

  def choose(lb: Int, ub: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map((v: Int) => lb + v % (ub - lb)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    double.flatMap(d => if (d < threshold) g1._1 else g2._1)
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (s => this.listOfN(s))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g(_) map f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n =>
      g(n) flatMap (f(_).g(n))
    SGen(g2)
  }


}

