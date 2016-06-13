package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Ardjan on 13/06/2016.
  */
class OptionSpec extends FlatSpec with Matchers {
  "Exercise 4.1" should "map" in {
    val s = Some(1)
    val n = None

    assert(s.map(_.toString) == Some("1"))
    assert(n.map(_.toString) == None)
  }

  "Exercise 4.1" should "getOrElse" in {
    val s = Some(1)
    val n = None

    assert(s.getOrElse(2) == 1)
    assert(n.getOrElse(2) == 2)
  }
  "Exercise 4.1" should "flatMap" in {
    val s = Some(1)
    val n = None

    assert(s.flatMap(_ => Some(2)) == Some(2))
    assert(s.flatMap(_ => None) == None)
    assert(n.flatMap(_ => Some(2)) == None)
    assert(n.flatMap(_ => None) == None)
  }
  "Exercise 4.1" should "orElse" in {
    val s = Some(1)
    val n = None

    assert(s.orElse(None) == s)
    assert(s.orElse(Some(2)) == s)
    assert(n.orElse(None) == n)
    assert(n.orElse(Some(2)) == Some(2))
  }

  "Exercise 4.1" should "filter" in {
    val s = Some(1)
    val n = None

    assert(s.filter( _ => true ) == s )
    assert(s.filter( _ => false ) == n )
    assert(n.filter( _ => true ) == n )
    assert(n.filter( _ => false ) == n )
    assert(s.filter( x => x == 1 ) == s )
  }

  "Exercise 4.2" should "variance" in {
    val s = List(1.0,-1.0)

    assert( Option.variance( s ) == Some(1.0))
    assert( Option.variance( List(0.0) ) == Some(0.0) )
    assert( Option.variance( Nil ) == None )
  }

  "Exercise 4.3" should "map2" in {
    assert( Option.map2(Some(1), Some(2))(_ + _ ) == Some(3))
    assert( Option.map2(None, Some(2))( (a,b) => b ) == None)
    assert( Option.map2(Some(1), None)( (a,b) => a ) == None)
    assert( Option.map2(None, None)( (a,b) => 1 ) == None)
    assert( Option.map2(Some(1), Some(2))((_,_)) == Some((1,2)))
  }

  "Exercise 4.4" should "sequence" in {
    val l = List(Some(1), Some(2), Some(3))
    val r = Some(List(1,2,3))

    assert( Option.sequence(l) == r )

    val n = List(Some(1),Some(2), None)

    assert( Option.sequence(n) == None )

    assert( Option.sequence(Nil) == Some(Nil) )
  }
}
