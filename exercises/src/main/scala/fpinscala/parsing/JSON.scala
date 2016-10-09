package fpinscala.parsing

import language.higherKinds

trait JSON

object JSON {
  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON

    val spaces = char(' ').many.slice

    def obj: Parser[JSON] = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap)))
    def array: Parser[JSON] = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq)))
    def keyval = quotedString ** (":" *> obj)
    def lit: Parser[JSON] =
      "null".as(JNull) |
      double.map(JNumber) |
      quotedString.map(JString) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))

    def value = array | obj | lit


    array | obj
  }
}
