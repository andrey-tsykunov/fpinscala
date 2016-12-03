package fpinscala.state

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class RNGTest extends FlatSpec with Matchers {

  import RNG._

  trait RngFixture {
    val seed = constRNG(0)

    def constRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, this)
    }

    def incrementRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, incrementRNG(i+1))
    }

    def verify[A](rand: Rand[A], expected: A) = rand(seed) shouldBe (expected, seed)
  }

  it should "work for nonNegativeInt" in new RngFixture {

    nonNegativeInt(constRNG(0))._1 should be (0)
    nonNegativeInt(constRNG(5))._1 should be (5)
    nonNegativeInt(constRNG(-5))._1 should be (5)
    nonNegativeInt(constRNG(Int.MinValue))._1 should be (Int.MaxValue)
    nonNegativeInt(constRNG(Int.MaxValue))._1 should be (Int.MaxValue)
  }

  it should "work for double" in new RngFixture {
    double(constRNG(0))._1 should be (0.0)
    double(constRNG(-100000))._1 should be (100000.toDouble / Int.MaxValue)

    double2(constRNG(0))._1 should be (0.0)
    double2(constRNG(-100000))._1 should be (100000.toDouble / Int.MaxValue)
  }

  it should "work for int" in new RngFixture {
    intDouble(incrementRNG(100))._1 should be ((100, 101.toDouble / Int.MaxValue))
  }

  it should "work for ints" in new RngFixture {
    ints(3)(incrementRNG(1))._1 should be (List(1, 2, 3))

    ints2_tailRec(3)(incrementRNG(1))._1 should be (List(1, 2, 3))
    ints2_tailRec(100000)(incrementRNG(1))._1 take 3 should be (List(1, 2, 3))

    ints3_sequence(3)(incrementRNG(1))._1 should be (List(1, 2, 3))
  }

  it should "work for positiveMax" in new RngFixture {
    positiveMax(2)(constRNG(100))._1 should be (0)
    positiveMax(2)(constRNG(Int.MaxValue))._1 should be (2)
  }

  it should "work for map" in new RngFixture {
    val add2 = map(unit(3))(_ + 2)

    verify(add2, 5)
  }

  it should "work for map2" in new RngFixture {
    val plus = map2(unit(3), unit(2))(_ + _)

    verify(plus, 5)
  }

  it should "work for sequence" in new RngFixture {

    val seq = sequence(List(unit(3), unit(1), unit(2)))

    verify(seq, List(3, 1, 2))
  }
}
