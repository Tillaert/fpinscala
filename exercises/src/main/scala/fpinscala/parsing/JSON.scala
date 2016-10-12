package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

   // val spaces = char(' ').many.slice

    def array: Parser[JSON] = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq)))
    def obj: Parser[JSON] = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap)))
    def keyval = escapedQuoted ** (":" *> value)
    def lit: Parser[JSON] =
      "null".as(JNull) |
      double.map(JNumber) |
        escapedQuoted.map(JString) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))

    def value: Parser[JSON] = lit | obj | array


    obj | array
  }
}
