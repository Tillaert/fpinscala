package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.parsing.ReferenceTypes.Failure
import fpinscala.testing._

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>
  // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def whitespace: Parser[String] = "\\s*".r

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = ("(.*?)"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
  token(quoted)

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble)

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    p //<* whitespace

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  implicit def regex(r: Regex): Parser[String]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  def furthest[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred most recently. */
  def latest[A](p: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List[A]())

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(v => succeed(f(v)))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      v <- p
      v2 <- p2
    } yield (v, v2)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      v <- p
      v2 <- p2
    } yield f(v, v2)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def skipL[A, B](p: Parser[A], p2: Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A, B](p: Parser[A], p2: Parser[B]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: Parser[A]) =
    start *> p <* stop

  def sep[A](p: Parser[A], p2: Parser[Any]) =
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def errorMessage(e: ParseError): String

  def errorLocation(e: ParseError): Location

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B) = self.map(p)(f)

    def product[B](p2: => Parser[B]) = self.product(p, p2)

    def **[B](p2: => Parser[B]) = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)

    def many = self.many(p)

    def slice = self.slice(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)

    def <*[B](p2: => Parser[B]) = self.skipR(p, p2)

    def sep(s: Parser[Any]) = self.sep(p, s)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  object Laws {

    import Prop.forAll

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLaw[A](p: Parser[A], p2: Parser[A], p3: Parser[A])(in: Gen[String]): Prop =
      equal((p ** p2) ** p3, p ** (p2 ** p3))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) =
    copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
  if (input.length > 1) input.lines.drop(line - 1).next
  else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}