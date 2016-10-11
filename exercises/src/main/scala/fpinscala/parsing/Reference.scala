package fpinscala.parsing

import ReferenceTypes._
import scala.util.matching.Regex

object ReferenceTypes {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def attempt[A](p: Parser[A]): Parser[A] =
      s => p(s).uncommit

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

}

object Reference extends Parsers[Parser] {
  def string(s: String): Parser[String] = {
    val msg = "'" + s + "'"
    l =>
      l.input.substring(l.offset).startsWith(s) match {
        case true => Success(s, s.length)
        case false => Failure(l.toError(msg), isCommitted = false)
    }
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => {
      r.findPrefixOf(s.input) match {
        case None => Failure(s.toError(msg), isCommitted = false)
        case Some(m) => Success(m, m.length)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] = s => Success(a,0)

  def slice[A](p: Parser[A]): Parser[String] =
    l => p(l) match {
      case Success(_, cs) => Success(l.input.substring(l.offset, l.offset + cs), cs)
      case f: Failure => f
    }

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def errorLocation(e: ParseError): Location = ???

  def errorMessage(e: ParseError): String = ???

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case f@Failure(_,_) => f
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case Failure(e, false) => p2(s)
      case r => r // committed failure or success skips running `p2`
    }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(Location(input)).extract
  }

  def furthest[A](p: Parser[A]): Parser[A] = ???

  def latest[A](p: Parser[A]): Parser[A] = ???

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }
}