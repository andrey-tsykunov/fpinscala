package fpinscala.laziness

import org.scalatest.{Matchers, FunSuite}

class StreamTest extends FunSuite with Matchers {

  test("testTakeWhile") {
    Stream(1, 2, 3).takeWhile(_ < 2).toList should be (List(1))
    Stream(1, 2, 3).takeWhile(_ < 1).toList should be (Nil)
    Stream(1, 2, 3).takeWhile(_ < 5).toList should be (List(1, 2, 3))
    Stream(1, 2, 3, 1, 2).takeWhile(_ < 2).toList should be (List(1))

    Stream.from(1).takeWhile(_ < 3).toList() should be (List(1, 2))
  }

  test("testTakeWhileViaFoldRight") {
    Stream(1, 2, 3).takeWhileViaFoldRight(_ < 2).toList should be (List(1))
    Stream(1, 2, 3).takeWhileViaFoldRight(_ < 1).toList should be (Nil)
    Stream(1, 2, 3).takeWhileViaFoldRight(_ < 5).toList should be (List(1, 2, 3))
    Stream(1, 2, 3, 4, 5, 1, 2).takeWhileViaFoldRight(_ < 2).toList should be (List(1))

    Stream.from(1).takeWhileViaFoldRight(_ < 3).toList() should be (List(1, 2))
  }

  test("testTake") {
    Stream(1, 2, 3).take(1).toList should be (List(1))
    Stream(1, 2, 3).take(3).toList should be (List(1, 2, 3))
    Stream(1, 2, 3).take(0).toList should be (Nil)
    Stream(1, 2, 3).take(4).toList should be (List(1, 2, 3))

    Stream.from(1).take(2).toList() should be (List(1, 2))
  }

  test("foldLeft") {
    Stream(1, 2, 3).foldLeft(0)(_ + _) should be (6)

    Stream(1, 2, 3, 4, 5, 6).foldLeft(0)((b, acc) => if (b < 4) b + acc else b) should be (6)

    // not lazy
    // Stream.from(1).foldLeft(0)((b, acc) => if (b < 4) b + acc else b) should be (6)
  }

  test("exists") {
    Stream(1, 2, 3).exists(_ == 2) should be (true)
    Stream(1, 2, 3).exists(_ == 4) should be (false)

    Stream.from(2).exists(_ == 2) should be (true)
  }

  test("foldRight") {
    Stream(1, 2, 3).foldRight(0)(_ + _) should be (6)

    Stream(1, 2, 3, 4, 1, 1).foldRight(0)((b, acc) => if (b < 4) b + acc else 0) should be (6)
    Stream.from(1).foldRight(0)((b, acc) => if (b < 4) b + acc else 0) should be (6)
  }

  test("testForAll") {
    Stream(1, 2, 3).forAll(_ < 4) should be (true)
    Stream(1, 2, 3).forAll(_ < 3) should be (false)

    Stream.from(1).forAll(_ < 3) should be (false)
  }

  test("testDrop") {
    Stream(1, 2, 3).drop(0).toList should be (List(1, 2, 3))
    Stream(1, 2, 3).drop(1).toList should be (List(2, 3))
    Stream(1, 2, 3).drop(3).toList should be (Nil)
    Stream(1, 2, 3).drop(4).toList should be (Nil)
  }

  test("testToList") {
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  test("testHeadOption") {
    Stream(1, 2, 3).headOption should be (Some(1))
    Stream().headOption should be (None)
  }

  test("testStartsWith") {
  }

  test("testFrom") {
    Stream.from(100).take(3).toList() should be (List(100, 101, 102))
  }

}
