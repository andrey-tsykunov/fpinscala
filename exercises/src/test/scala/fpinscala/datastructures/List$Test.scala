package fpinscala.datastructures

import org.scalatest.{Matchers, FunSuite}

class List$Test extends FunSuite with Matchers {

  test("testDrop") {

    List.drop(List(1), 0) should be (List(1))
    List.drop(List(1), 1) should be (Nil)
    List.drop(List(1, 2, 3), 2) should be (List(3))
    List.drop(List(1, 2, 3), 3) should be (Nil)

    List.drop(Nil, 0) should be (Nil)

    intercept[UnsupportedOperationException] { List.drop(Nil, 1) }

    intercept[IllegalArgumentException] { List.drop(List(1), -1) }
  }

  test("testTail") {
    List.tail(List(1)) should be (Nil)
    List.tail(List(1, 2, 3)) should be (List(2, 3))

    intercept[UnsupportedOperationException] { List.tail(Nil) }
  }

  test("testDropWhile") {

    List.dropWhile(Nil: List[Int], (x:Int) => x > 0) should be (Nil)

    List.dropWhile(List(1, 2, 3), (x:Int) => x > 0) should be (Nil)
    List.dropWhile(List(1, 2, 3), (x:Int) => x < 0) should be (List(1, 2, 3))

    List.dropWhile(List(1, 2, 3, -1, 2), (x:Int) => x > 0) should be (List(-1, 2))

    List.dropWhile(List(1 to 100000: _*), (x:Int) => x < 0)
  }

  test("testSetHead") {

    intercept[UnsupportedOperationException] { List.setHead(Nil: List[Int], 1) }

    List.setHead(List(1), 2) should be (List(2))
    List.setHead(List(1, 2, 3), 5) should be (List(5, 2, 3))
  }

  test("testInit") {

    intercept[UnsupportedOperationException] { List.init(Nil) }

    List.init(List(1)) should be (Nil)
    List.init(List(1, 2, 3)) should be (List(1, 2))
  }

}
