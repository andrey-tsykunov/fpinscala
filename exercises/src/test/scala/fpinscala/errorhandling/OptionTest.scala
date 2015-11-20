package fpinscala.errorhandling

import org.scalatest.{Matchers, FunSuite}

class Base(val data: String)

class Derived(data: String) extends Base(data)

class OptionTest extends FunSuite with Matchers {
  val x = new Derived("X")
  val y = new Base("Y")

  test("map") {
    Some(x).map(_.data) should be (Some("X"))
    Some(x).flatMap(x => Some(x.data)) should be (Some("X"))
  }

  test("getOrElse") {
    Some(x).getOrElse(y) should be (x)
    None.getOrElse(y) should be (y)
  }

  test("orElse") {
    Some(x).orElse(Some(y)) should be (Some(x))
    None.orElse(Some(y)) should be (Some(y))
    Some(x).orElse(None) should be (Some(x))
    None.orElse(None) should be (None)
  }

  test("filter") {
    Some(x).filter(_.data != "X") should be (None)
    Some(x).filter(_.data == "X") should be (Some(x))

    val none: Option[Base] = None
    none.filter(_.data != "X") should be (None)
  }

  test("variance") {
    Option.variance(Seq()) should be (None)
    Option.variance(Seq(2.0)) should be (Some(0.0))
  }

  test("map2") {
    Option.map2(Some("a"), Some("b"))(_ + _) should be (Some("ab"))
    Option.map2(Some("a"), None)(_ + _) should be (None)
    Option.map2(None: Option[String], Some("a"))(_ + _) should be (None)
  }

  test("sequence") {
    Option.sequence(List(Some("a"), Some("b"))) should be (Some(List("a", "b")))
    Option.sequence(List(Some("a"), None)) should be (None)
  }

  test("traverse") {
    Option.sequenceViaTranverse(List(Some("a"), Some("b"))) should be (Some(List("a", "b")))
    Option.sequenceViaTranverse(List(Some("a"), None)) should be (None)

    Option.traverseViaFoldRight(List(Some("a"), Some("b")))(x => x) should be (Some(List("a", "b")))
  }
}
