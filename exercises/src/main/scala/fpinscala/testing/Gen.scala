package fpinscala.testing

import fpinscala.state.State
import fpinscala.state.RNG
import fpinscala.testing.Prop.{FailedCase, SuccessCount}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def &&(that: Prop): Prop = new Prop {
    override def check =
      (Prop.this.check, that.check) match {
        case (Right(l), Right(r)) => Right(l + r)
        case (Right(l), Left((f,r))) => Left((f,l+r))
        case (Left((f,l)), Right(r)) => Left((f,l+r))
        case (Left((fl,l)), Left((fr,r))) => Left((fl+fr,l+r))
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def choose(lb: Int, ub: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map( (v:Int) => lb + v % (ub - lb ) ))
}

case class Gen[A](sample: State[RNG, A]) {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

