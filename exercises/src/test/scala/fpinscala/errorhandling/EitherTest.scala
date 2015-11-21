package fpinscala.errorhandling

import org.scalatest.{Matchers, FunSuite}

class EitherTest extends FunSuite with Matchers {

  test("testMap2") {
    Right("a").map2(Right("b"))(_ + _) should be (Right("ab"))
    Right("a").map2(Left(1))(_ + _) should be (Left(1))
    (Left(1): Either[Int, String]).map2(Right("b"))(_ + _) should be (Left(1))
    (Left(1): Either[Int, String]).map2(Left(2))(_ + _) should be (Left(1))
  }

  test("testFlatMap") {
    Right("a").flatMap(x => Right(x + "1")) should be (Right("a1"))

    (Left(1): Either[Int, String]).map(x => Right(x + "1")) should be (Left(1))
  }

  test("testOrElse") {
    Right("a").orElse(Right("b")) should be (Right("a"))

    (Left(1): Either[Int, String]).orElse(Right("b")) should be (Right("b"))
  }

  test("testMap") {
    Right("a").map(x => x.toUpperCase) should be (Right("A"))

    (Left(1): Either[Int, String]).map(x => x.toUpperCase) should be (Left(1))
  }

  test("sequence") {
    Right("a").map(x => x.toUpperCase) should be (Right("A"))

    Either.sequence(List(Right("a"), Right("b"))) should be (Right(List("a", "b")))
    Either.sequence(List(Right("a"), Left(1))) should be (Left(1))
    Either.sequence(List(Left(1), Right("a"))) should be (Left(1))
    Either.sequence(List(Left(1), Left(2))) should be (Left(1))
  }
}
