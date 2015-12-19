package fpinscala.laziness

import org.scalatest.{Matchers, FunSuite}

class StreamTest extends FunSuite with Matchers {

  test("testTakeWhile") {
    Stream(1, 2, 3).takeWhile(_ < 2).toList should be (List(1))
    Stream(1, 2, 3).takeWhile(_ < 1).toList should be (Nil)
    Stream(1, 2, 3).takeWhile(_ < 5).toList should be (List(1, 2, 3))
    Stream(1, 2, 3, 1, 2).takeWhile(_ < 2).toList should be (List(1))

    Stream.from(1).takeWhile(_ < 3).toList() should be (List(1, 2))

    Stream(1, 2, 3).takeWhileViaUnfold(_ < 2).toList should be (List(1))
    Stream.from(1).takeWhileViaUnfold(_ < 3).toList() should be (List(1, 2))
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

    Stream(1, 2, 3).takeViaUnfold(1).toList should be (List(1))
    Stream.from(1).takeViaUnfold(2).toList() should be (List(1, 2))
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

    Stream(1, 2, 3, 4, 1, 1).foldRight(100 /* not used */)((b, t) => if (b < 4) b + t else 0) should be (6)
    Stream.from(1).foldRight(100 /* not used */)((b, t) => if (b < 4) b + t else 0) should be (6)
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
    Stream.from(1).take(3).toList() should be (List(1, 2, 3))
    Stream.fromViaUnfold(1).take(3).toList() should be (List(1, 2, 3))
  }

  test("constant") {
    Stream.constant(1).take(3).toList() should be (List(1, 1, 1))
  }

  test("testMap") {

    Stream(1, 2, 3).map(_ + 1).toList() should be (List(2, 3, 4))
    Stream.from(1).map(_ + 1).take(3).toList() should be (List(2, 3, 4))

    Stream(1, 2, 3).mapViaUnfold(_ + 1).toList() should be (List(2, 3, 4))
    Stream.from(1).mapViaUnfold(_ + 1).take(3).toList() should be (List(2, 3, 4))
  }

  test("testFilter") {

    Stream(1, 2, 3).filter(_ < 3).toList() should be (List(1, 2))

    // does not work if _ < 3
    Stream.from(1).filter(_ < 4)
      .take(2)
      .toList() should be (List(1, 2))
  }

  test("testAppend") {

    Stream(1, 2, 3).append(Stream(0)).toList() should be (List(1, 2, 3, 0))
  }

  test("testFlatMap") {

    Stream(1, 2, 3).flatMap(Stream(0, _)).toList() should be (List(0, 1, 0, 2, 0, 3))

    Stream.from(1).flatMap(Stream(0, _)).take(6).toList() should be (List(0, 1, 0, 2, 0, 3))
  }

  test("testFib") {

    Stream.fibs().take(7).toList() should be (List(0, 1, 1, 2, 3, 5, 8))
    Stream.fibsViaUnfold().take(7).toList() should be (List(0, 1, 1, 2, 3, 5, 8))

    // not tail rec...
    //Stream.fib().take(10000).toList().take(7) should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  test("zip") {
    Stream(1,2,3).zip(Stream(4,5,6)).toList() should be (List((1, 4), (2,5), (3,6)))

    Stream(1,2,3).zip(Stream(4,5)).toList() should be (List((1, 4), (2,5)))

    Stream(1,2).zip(Stream(4,5,6)).toList() should be (List((1, 4), (2,5)))

    Stream(1,2,3).zip(Empty).toList() should be (Nil)

    Empty.zip(Stream(1,2,3)).toList() should be (Nil)
    Empty.zip(Empty).toList() should be (Nil)
  }

  test("zipAll") {
    Stream(1,2,3).zipAll(Stream(4,5,6)).toList() should be (List((Some(1), Some(4)), (Some(2),Some(5)), (Some(3),Some(6))))

    Stream(1,2,3).zipAll(Stream(4,5)).toList() should be (List((Some(1), Some(4)), (Some(2),Some(5)), (Some(3),None)))

    Stream(1,2).zipAll(Stream(4,5,6)).toList() should be (List((Some(1), Some(4)), (Some(2),Some(5)), (None,Some(6))))

    Stream(1,2,3).zipAll(Empty).toList() should be (List((Some(1), None), (Some(2),None), (Some(3),None)))

    Empty.zipAll(Stream(4,5,6)).toList() should be (List((None, Some(4)), (None,Some(5)), (None,Some(6))))
    Empty.zipAll(Empty).toList() should be (Nil)
  }

  test("startsFrom") {
    Stream(1, 2, 3).startsFrom(Stream(1, 2)) should be (true)
    Stream(1, 2, 3).startsFrom(Stream(1, 2, 3)) should be (true)
    Stream(1, 2, 3).startsFrom(Stream(1)) should be (true)
    Stream(1, 2, 3).startsFrom(Stream()) should be (true)

    Stream(1, 2, 3).startsFrom(Stream(2)) should be (false)
    Stream(1, 2, 3).startsFrom(Stream(1, 1)) should be (false)
    Stream(1, 2, 3).startsFrom(Stream(1, 2, 1)) should be (false)
    Stream(1, 2, 3).startsFrom(Stream(1, 2, 3, 4)) should be (false)

    Stream.from(1).startsFrom(Stream(1, 2)) should be (true)
    Stream.from(1).startsFrom(Stream(1, 2, 4)) should be (false)
  }

  test("startsWith") {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be (true)
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3)) should be (true)
    Stream(1, 2, 3).startsWith(Stream(1)) should be (true)
    Stream(1, 2, 3).startsWith(Stream()) should be (true)

    Stream(1, 2, 3).startsWith(Stream(2)) should be (false)
    Stream(1, 2, 3).startsWith(Stream(1, 1)) should be (false)
    Stream(1, 2, 3).startsWith(Stream(1, 2, 1)) should be (false)
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) should be (false)

    Stream.from(1).startsWith(Stream(1, 2)) should be (true)
    Stream.from(1).startsWith(Stream(1, 2, 4)) should be (false)
  }

  test("tails") {
    Stream(1, 2, 3).tails.toList().map(x => x.toList()) should be (List(
      List(1,2,3),
      List(2,3),
      List(3),
      Nil
    ))
  }

  test("scanRight") {
    Stream(1,2,3).scanRight(0)(_ + _).toList should be (List(6, 5, 3, 0))
  }

}
