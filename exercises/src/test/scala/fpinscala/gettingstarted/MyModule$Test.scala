package fpinscala.gettingstarted

import org.scalatest.{Matchers, FunSuite}

import MyModule._

class MyModule$Test extends FunSuite with Matchers {

  test("testFib") {
    fib(0) should be (0)
    fib(1) should be (1)
    fib(2) should be (1)
    fib(3) should be (2)
    fib(4) should be (3)
    fib(5) should be (5)

    fib(10000)

    intercept[IllegalArgumentException] { fib(-1) }
  }

}
