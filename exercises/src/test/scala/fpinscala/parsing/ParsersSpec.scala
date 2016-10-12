package fpinscala.parsing

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.testing._

class ParsersSpec extends FlatSpec with Matchers {

  val P = fpinscala.parsing.Reference
  //val json = JSON.jsonParser(P)

  "doubleString" should "be parsed" in {
    val doubleTxt = "3.14"

    P.run(P.doubleString)(doubleTxt) should be(Right("3.14"))
  }

  "double" should "be parsed" in {
    val doubleTxt = "3.14"
    P.run(P.double)(doubleTxt) should be(Right(3.14))
  }

  "string" should "be parsed" in {
    val stringTxt = "All the kings horses"
    P.run(P.string("All"))(stringTxt) should be( Right("All"))
  }

  "regex" should "equal match" in {
    val jsonTxt = """[foobar]"""
    P.run(P.regex("^\\[([^\\]]*)\\]".r))(jsonTxt) should be( Right("[foobar]"))
  }

  "quotedString" should "equal match" in {
    val quotedStr = "\"foobar\""
    P.run(P.quoted)(quotedStr) should be( Right("foobar"))
  }

  "mapLaw" should "apply to P.double" in {
    val doubleGen = Gen.double.map(_.toString)
    Prop.run(Reference.Laws.mapLaw(P.double)(doubleGen))
  }

  /*
  "SimpleObject" should " equal Right(k:v)" in {
    val jsonTxt = """{"foo":"bar"}"""
    P.run(P.JSON)(jsonTxt) == Right(Map(("foo", "Bar")))
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

