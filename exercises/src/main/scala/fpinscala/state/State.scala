package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (a, _)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    (if(i == Int.MinValue) Int.MaxValue else i.abs, rng2)
  }

  def nonNegativeInt_retry(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    if(i == Int.MinValue) nonNegativeInt(rng2) else (i.abs, rng2)
  }

  def nonNegativeInt_map: Rand[Int] = map(int){ i =>
    if (i != Int.MinValue) i.abs else Int.MaxValue
  }

  def positiveMax(n: Int): Rand[Int] = map(nonNegativeInt)(i => (i.toDouble * n / Int.MaxValue).toInt)

  def double(rng: RNG): (Double, RNG) = {
    val (i,rng2) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng2)
  }

  def double2: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,rng2) = rng.nextInt
    val (d, rng3) = double(rng2)

    ((i, d), rng3)
  }

  def intDouble2(rng: RNG): ((Int,Double), RNG) = map2(int, double)((_, _))(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (v, r) = intDouble(rng)

    (v.swap, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case x => {
      val (i, rng2) = rng.nextInt
      val (xs, rnd3) = ints(count - 1)(rng2)
      (i :: xs, rnd3)
    }
  }

  def ints2_tailRec(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def generate(count: Int, rng: RNG, prev: List[Int]) : (List[Int], RNG) = count match {
      case 0 => (prev, rng)
      case x => {
        val (i, rng2) = rng.nextInt

        generate(count - 1, rng2, i :: prev)
      }
    }

    val r = generate(count, rng, Nil)
    (r._1.reverse, r._2)
  }

  def ints3_sequence(count: Int)(rng: RNG): (List[Int], RNG) = {

    sequence(List.fill(count)(int))(rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {

    val r = fs.foldLeft((List.empty[A], rng)) {
      case ((ls, rng), rand) => {
        val (a, nextRng) = rand(rng)

        (a :: ls, nextRng)
      }
    }

    (r._1.reverse, r._2)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
