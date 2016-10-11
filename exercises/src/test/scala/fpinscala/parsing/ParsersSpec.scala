package fpinscala.parsing

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.testing._


class ParsersSpec extends FlatSpec with Matchers {
  import fpinscala.parsing.ReferenceTypes.Parser

  val P = fpinscala.parsing.Reference
 // val json: Parser[JSON] = JSON.jsonParser(P)

  "double" should "be parsed" in {
    val doubleTxt = "3.14"
    P.run(P.doubleString)(doubleTxt) == Right("3.14")
  }

 /* "SimpleArray" should " equal Right(c)" in {
    val jsonTxt = """["foobar"]"""
    P.run(json)(jsonTxt) == Right(List("foobar"))
  }*/

  /*"SimpleObject" should " equal Right(k:v)" in {
    val jsonTxt = """{"foo":"bar"}"""
    P.run(json)(jsonTxt) == Right(Map(("foo", "Bar")))
  }*/

  "or" should "resolve right hand side" in {
    // run("abra" | "cadabra")("abra") == Right("cadabra")
  }

  "listOfN" should "resolve" in {
    // List("ababcad", "cadabab", "ababab").forall { s =>
    //   run(listOfN(3, "ab" | "cad"))(s) == Right(s)
    // }
  }


}

