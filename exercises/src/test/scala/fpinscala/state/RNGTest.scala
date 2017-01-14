package fpinscala.state

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class RNGTest extends FlatSpec with Matchers {

  import RNG._

  trait RngFixture {
    val zeroSeed = constRNG(0)

    val incrementSeed = incrementRNG(1)

    def constRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, constRNG(i))
    }

    def incrementRNG(i: Int): RNG = new RNG {
      override def nextInt: (Int, RNG) = (i, incrementRNG(i+1))
    }

    def const(i: Int): Rand[Int] = _ => constRNG(i).nextInt

    def increment(i: Int): Rand[Int] = _ => incrementRNG(i).nextInt

    def verify[A](rands: Rand[A]*)(expected: A, seed: RNG): Unit = {

      rands.foreach { rand =>
        val (value, next) = rand(seed)

        value shouldBe expected
        next should not be (seed)
      }
    }
  }

  it should "work for int" in new RngFixture {

    verify(RNG.int)(1, incrementSeed)
  }

  it should "work for nonNegativeInt" in new RngFixture {

    verify(nonNegativeInt)(0, zeroSeed)
    verify(nonNegativeInt)(5, constRNG(5))
    verify(nonNegativeInt)(5, constRNG(-5))
    verify(nonNegativeInt)(Int.MaxValue, constRNG(Int.MinValue))
    verify(nonNegativeInt)(Int.MaxValue, constRNG(Int.MaxValue))
  }

  it should "work for double" in new RngFixture {

    val rands = verify(double, double2)_

    rands(0.0, zeroSeed)
    rands(100000.toDouble / Int.MaxValue, constRNG(-100000))
  }

  it should "work for intDouble" in new RngFixture {

    val rands = verify(intDouble, intDouble_map2)_

    rands((1, 2.toDouble / Int.MaxValue), incrementSeed)
  }

  it should "work for doubleInt" in new RngFixture {

    val rands = verify(doubleInt, doubleInt_map)_

    rands((2.toDouble / Int.MaxValue, 1), incrementSeed)
  }

  it should "work for ints" in new RngFixture {

    val rands = verify(ints(3), ints_tailRec(3), ints_sequence(3))_

    rands(List(1, 2, 3), incrementSeed)

    ints_tailRec(100000)(incrementSeed)._1 take 3 should be (List(1, 2, 3))
  }

  it should "work for positiveMax" in new RngFixture {

    val rands = verify(positiveMax(2))_

    rands(0, constRNG(100))
    rands(2, constRNG(Int.MaxValue))
  }

  it should "work for map" in new RngFixture {

    val add2  = map(const(3))(_ + 2)

    verify(add2)(5, zeroSeed)
  }

  it should "work for map2" in new RngFixture {

    val plus = map2(const(3), unit(2))(_ + _)

    verify(plus)(5, zeroSeed)
  }

  it should "work for sequence" in new RngFixture {

    val ls = List(const(3), const(1), const(2))

    val seq = sequence(ls)
    val seq_map = sequence_map(ls)

    verify(seq, seq_map)(List(3, 1, 2), zeroSeed)
  }

  it should "work for flatMap" in new RngFixture {

    val add1 = flatMap(const(3))(x => const(x + 1))

    verify(add1)(4, zeroSeed)
  }
}
