package fpinscala.gettingstarted

import org.scalatest.{Matchers, FunSuite}

import PolymorphicFunctions._

class PolymorphicFunctions$Test extends FunSuite with Matchers {

  test("testIsSorted") {
    isSorted(Array(), gtInt) should be (true)
    isSorted(Array(1), gtInt) should be (true)

    isSorted(Array(1,2), gtInt) should be (true)
    isSorted(Array(2,1), gtInt) should be (false)

    isSorted(Array(1,2,3), gtInt) should be (true)
    isSorted(Array(1,3,2), gtInt) should be (false)

    isSorted(Array(1,1), gtInt) should be (true)
  }

  private def gtInt(x: Int, y: Int) = x >= y

  test("testPartial1") {
    val f = partial1(1, (a: Int, b: Int) => a + b)

    f(2) should be (3)
  }

  test("testCurry") {
    val f = curry((a: Int, b: Int) => a + b)

    f(2)(3) should be (5)
  }

  test("testUncurry") {

    val f = uncurry((a: Int) => (b: Int) => a + b)

    f(2, 3) should be (5)
  }

  test("testCompose") {

    val f = compose((a: Int) => a + 1, (b: Int) => b * 2)

    f(3) should be (7)
  }

}
