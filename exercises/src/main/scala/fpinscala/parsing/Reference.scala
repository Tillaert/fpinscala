package fpinscala.parsing

import ReferenceTypes._
import scala.util.matching.Regex

object ReferenceTypes {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]

  case class Success[+A](get: A, charsConumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

}

object Reference extends Parsers[Parser] {
  def string(s: String): Parser[String] = {
    val msg = "'" + s + "'"
    l => l.input.substring(l.offset).startsWith(s) match {
      case true => Success(s, s.length)
      case false => Failure(l.toError(msg))
    }
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => {
      r.findPrefixOf(s.input) match {
        case None => Failure(s.toError(msg))
        case Some(m) => Success(m, m.length)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] = succeed(a)

  def slice[A](p: Parser[A]): Parser[String] =
    l => p(l) match {
      case Success( _, cs ) => Success( l.input.substring(l.offset, l.offset + cs), cs )
      case f:Failure => f
    }
}