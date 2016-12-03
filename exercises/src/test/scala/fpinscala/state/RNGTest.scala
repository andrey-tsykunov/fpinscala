package fpinscala.state

import fpinscala.state.RNG.{Rand, Simple}
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class RNGTest extends FlatSpec with Matchers {

  trait RngFixture {
    def constRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, this)
    }

    def incrementRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, incrementRNG(i+1))
    }
  }

  trait RandFixture extends RngFixture {
    val seed = constRNG(0)

    def verify[A](rand: Rand[A], expected: A) = {
      rand(seed) shouldBe (expected, seed)
    }

    def const[A](a: A): Rand[A] = r => (a, r)
  }

  it should "work for nonNegativeInt" in new RngFixture {

    RNG.nonNegativeInt(constRNG(0))._1 should be (0)
    RNG.nonNegativeInt(constRNG(5))._1 should be (5)
    RNG.nonNegativeInt(constRNG(-5))._1 should be (5)
    RNG.nonNegativeInt(constRNG(Int.MinValue))._1 should be (Int.MaxValue)
    RNG.nonNegativeInt(constRNG(Int.MaxValue))._1 should be (Int.MaxValue)
  }

  it should "work for double" in new RngFixture {
    RNG.double(constRNG(0))._1 should be (0.0)
    RNG.double(constRNG(-100000))._1 should be (100000.toDouble / Int.MaxValue)

    RNG.double2(constRNG(0))._1 should be (0.0)
    RNG.double2(constRNG(-100000))._1 should be (100000.toDouble / Int.MaxValue)
  }

  it should "work for int" in new RngFixture {
    RNG.intDouble(incrementRNG(100))._1 should be ((100, 101.toDouble / Int.MaxValue))
  }

  it should "work for ints" in new RngFixture {
    RNG.ints(3)(incrementRNG(1))._1 should be (List(1, 2, 3))

    RNG.intsWithSafeStack(3)(incrementRNG(1))._1 should be (List(3, 2, 1))

    RNG.intsWithSafeStack(100000)(incrementRNG(1))._1 take 3 should be (List(100000, 99999, 99998))
  }

  it should "work for positiveMax" in new RngFixture {
    RNG.positiveMax(2)(constRNG(100))._1 should be (0)
    RNG.positiveMax(2)(constRNG(Int.MaxValue))._1 should be (2)
  }

  it should "work for map" in new RandFixture {
    val add2 = RNG.map(const(3))(_ + 2)

    verify(add2, 5)
  }

  it should "work for map2" in new RandFixture {
    val plus = RNG.map2(const(3), const(2))(_ + _)

    verify(plus, 5)
  }

  it should "work for sequence" in new RandFixture {

    val seq = RNG.sequence(List(const(3), const(1), const(2)))

    verify(seq, List(3, 1, 2))
  }
}
