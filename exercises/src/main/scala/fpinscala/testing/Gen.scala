package fpinscala.testing

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean = ???

  def &&(that: Prop): Prop = new Prop{ override def check = Prop.this.check && that.check }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  def listOf[A](g: Gen[A]): Gen[List[A]] = ???

  def choose[Int](lb: Int, ub: Int): Gen[Int] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

