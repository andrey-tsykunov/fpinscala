package fpinscala.datastructures

import org.scalatest.{Matchers, FunSuite}

import List._

class ListTest extends FunSuite with Matchers {

  test("testDrop") {

    drop(List(1), 0) should be (List(1))
    drop(List(1), 1) should be (Nil)
    drop(List(1, 2, 3), 2) should be (List(3))
    drop(List(1, 2, 3), 3) should be (Nil)

    drop(Nil, 0) should be (Nil)

    intercept[UnsupportedOperationException] { drop(Nil, 1) }

    intercept[IllegalArgumentException] { drop(List(1), -1) }
  }

  test("testTail") {
    tail(List(1)) should be (Nil)
    tail(List(1, 2, 3)) should be (List(2, 3))

    intercept[UnsupportedOperationException] { tail(Nil) }
  }

  test("testDropWhile") {

    dropWhile(Nil: List[Int], (x:Int) => x > 0) should be (Nil)

    dropWhile(List(1, 2, 3), (x:Int) => x > 0) should be (Nil)
    dropWhile(List(1, 2, 3), (x:Int) => x < 0) should be (List(1, 2, 3))

    dropWhile(List(1, 2, 3, -1, 2), (x:Int) => x > 0) should be (List(-1, 2))

    dropWhile(List(1 to 100000: _*), (x:Int) => x < 0)
  }

  test("testSetHead") {

    intercept[UnsupportedOperationException] { setHead(Nil: List[Int], 1) }

    setHead(List(1), 2) should be (List(2))
    setHead(List(1, 2, 3), 5) should be (List(5, 2, 3))
  }

  test("testInit") {

    intercept[UnsupportedOperationException] { init(Nil) }

    init(List(1)) should be (Nil)
    init(List(1, 2, 3)) should be (List(1, 2))
  }

  test("foldRight") {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1, 2, 3))
    foldRight(List(1,2,3), new StringBuilder)((i, sb) => sb.append(i)).toString should be ("321")
  }

  test("foldLeft") {
    foldLeft(List(1,2,3), new StringBuilder)(_.append(_)).toString should be ("123")
  }

  test("foldRight via foldLeft") {

    foldRightViaFoldLeft(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1, 2, 3))
  }

  test("foldRight via foldLeft without Reverse") {

    foldRightViaFoldLeftWithoutReverse(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1, 2, 3))
  }

  test("foldLeft via foldRight without Reverse") {

    foldLeftViaFoldRightWithoutReverse(List(1,2,3), new StringBuilder)(_.append(_)).toString should be ("123")
  }

  test("append vial foldRight") {

    append(List(1,2,3), List(4,5)) should be (List(1,2,3,4,5))
  }

  test("foldLeft is tail recursive") {

    foldLeft(List(1 to 100000: _*), 0)(_ + _)
  }

  test("concat") {

    concat(List(List(1,2,3), List(4,5), List(6))) should be (List(1,2,3,4,5,6))
  }

  test("length") {
    List.length(List(1,2,3)) should be (3)
    List.length(Nil) should be (0)
  }

  test("reverse") {

    reverse(List(1, 2, 3)) should be (List(3, 2, 1))
    reverse(List(1)) should be (List(1))
    reverse(Nil) should be (Nil)
  }

  test("addOne") {
    addOne(List(1,2,3)) should be (List(2,3,4))
  }

  test("doubleToString") {
    doubleToString(List(1.1, 2.3)) should be (List("1.1", "2.3"))
  }

  test("map") {
    map(List(1,2,3))(_+1) should be (List(2,3,4))
  }

  test("map with safe stack") {

    map(List(1,2,3))(_+1) should be (List(2,3,4))

    mapWithStackSafe(List(1 to 100000: _*))(_+1)
  }

  test("filter") {
    filter(List(1,2,3,4,5))(_%2 == 1) should be (List(1,3,5))
  }

  test("filterWithFlatMap") {
    filter(List(1,2,3,4,5))(_%2 == 1) should be (List(1,3,5))
  }

  test("flatMap") {
    flatMap(List("ab", "cd"))(c => List(c: _*)) should be (List('a', 'b', 'c', 'd'))
  }

  test("zipIntegers") {
    zipIntegers(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
    zipIntegers(List(1,2,3), List(4,5)) should be (List(5,7))
    zipIntegers(List(1,2), List(4,5,6)) should be (List(5,7))
    zipIntegers(List(1,2,3), Nil) should be (Nil)
    zipIntegers(Nil, List(1,2,3)) should be (Nil)
    zipIntegers(Nil, Nil) should be (Nil)
  }

  test("zip") {
    zip(List(1,2,3), List(4,5,6))(_+_) should be (List(5,7,9))
    zip(List(1,2,3), List(4,5))(_+_) should be (List(5,7))
    zip(List(1,2), List(4,5,6))(_+_) should be (List(5,7))
    zip(List(1,2,3), Nil)(_+_) should be (Nil)
    zip(Nil:List[Int], List(1,2,3))(_+_) should be (Nil)
    zip(Nil:List[Int], Nil:List[Int])(_+_) should be (Nil)
  }

  test("zip with safe stack") {
    zipWithStackSafe(List(1,2,3), List(4,5,6))(_+_) should be (List(5,7,9))
    zipWithStackSafe(List(1 to 100000: _*), List(1 to 100000: _*))(_+_)
  }

  test("startsFrom") {
    startsFrom(List(1,2,3,4), List(1,2,3)) should be (true)
    startsFrom(List(1,2,3,4), Nil) should be (true)

    startsFrom(List(1,2,3,4), List(2,3,4)) should be (false)
    startsFrom(List(1,2,3,4), List(1,1)) should be (false)
    startsFrom(List(1,2,3,4), List(1,2,3,4,5)) should be (false)
  }

  test("hasSubsequence") {
    hasSubsequence(List(1,2,3,4), List(1,2,3)) should be (true)
    hasSubsequence(List(1,2,3,4), List(2,3,4)) should be (true)
    hasSubsequence(List(1,2,3,4), List(1,2)) should be (true)
    hasSubsequence(List(1,2,3,4), List(2,3)) should be (true)
    hasSubsequence(List(1,2,3,4), List(4)) should be (true)

    hasSubsequence(List(1,2,3,4), List(1,2,3,4)) should be (true)
    hasSubsequence(List(1,2,3,4), Nil) should be (true)

    hasSubsequence(List(1,2,3,4), List(1,1)) should be (false)
    hasSubsequence(List(1,2,3,4), List(1,3)) should be (false)
    hasSubsequence(List(1,2,3,4), List(2,1)) should be (false)
    hasSubsequence(List(1,2,3,4), List(5)) should be (false)

    hasSubsequence(List(1,2,3,4), List(1,2,3,4,5)) should be (false)
    hasSubsequence(List(1,2,3,4), List(3,4,5)) should be (false)
  }
}