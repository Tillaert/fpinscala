package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG
import fpinscala.testing.Prop.{FailedCase, Result, SuccessCount, TestCases}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: TestCases => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def &&(that: Prop): Prop = ???
}

object Prop {
  type TestCases = Int
  type Result = Either[(FailedCase, SuccessCount), SuccessCount]

  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(lb: Int, ub: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map((v: Int) => lb + v % (ub - lb)))
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(this.sample.map( a => f(a)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap( a => f(a).sample))

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap( s => this.listOfN(s))
}

trait SGen[+A] {

}

