package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{Matchers, FunSuite}

class RNGTest extends FunSuite with Matchers{

  test("nonNegativeInt") {

    RNG.nonNegativeInt(const(0))._1 should be (0)
    RNG.nonNegativeInt(const(5))._1 should be (5)
    RNG.nonNegativeInt(const(-5))._1 should be (5)
    RNG.nonNegativeInt(const(Int.MinValue))._1 should be (Int.MaxValue)
    RNG.nonNegativeInt(const(Int.MaxValue))._1 should be (Int.MaxValue)
  }

  test("double") {
    RNG.double(const(0))._1 should be (0.0)
    RNG.double(const(-100000))._1 should be (100000.toDouble / Int.MaxValue)

    RNG.double2(const(0))._1 should be (0.0)
    RNG.double2(const(-100000))._1 should be (100000.toDouble / Int.MaxValue)
  }

  test("intDouble") {
    RNG.intDouble(incrementRNG(100))._1 should be ((100, 101.toDouble / Int.MaxValue))
  }

  test("ints") {
    RNG.ints(3)(incrementRNG(1))._1 should be (List(1, 2, 3))

    RNG.intsWithSafeStack(3)(incrementRNG(1))._1 should be (List(3, 2, 1))

    RNG.intsWithSafeStack(100000)(incrementRNG(1))._1 take 3 should be (List(100000, 99999, 99998))
  }

  test("positiveMax") {
    RNG.positiveMax(2)(const(100))._1 should be (0)
    RNG.positiveMax(2)(const(Int.MaxValue))._1 should be (2)
  }

  def const(i: Int): RNG = new RNG {
    override def nextInt: (Int, RNG) = (i, this)
  }

  def incrementRNG(i: Int): RNG = new RNG {
    override def nextInt: (Int, RNG) = (i, incrementRNG(i+1))
  }
}
