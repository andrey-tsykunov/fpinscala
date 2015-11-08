package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def applyWthStackOverflow[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def apply[A](as: A*): List[A] = {
    var result: List[A] = Nil

    for {
      a <- as.reverse
    }
      result = Cons(a, result)

    result
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(x, tail) => Cons(h, tail)
    case Nil => throw new UnsupportedOperationException("setHead of empty list")
  }

  @tailrec
  def drop[A](l: List[A], n: Int) : List[A] = n match {
    case 0 => l
    case x if x < 0 => throw new IllegalArgumentException()
    case _ => l match {
      case Nil => throw new UnsupportedOperationException(s"drop($n) of empty list")
      case Cons(x, tail) => drop(tail, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, tail) => if(!f(x)) l else dropWhile(tail, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, tail) => Cons(x, init(tail))
    case Nil => throw new UnsupportedOperationException("init of empty list")
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, i) => i + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((i, _) => i + 1)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((l, i) => Cons(i, l))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((acc, i) => f(i, acc))

  def foldRightViaFoldLeftWithoutReverse[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b: B) => b)((acc, i) => (b: B) => acc(f(i, b)))(z)

  def foldLeftViaFoldRightWithoutReverse[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((i, acc) => (b: B) => acc(f(b, i)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](ll: List[List[A]]) : List[A] =
    foldRight(ll, Nil:List[A])(append)

  def addOne(l: List[Int]):List[Int] =
    foldRight(l, Nil:List[Int])((i, acc) => Cons(i + 1, acc))

  def doubleToString(l: List[Double]):List[String] =
    foldRight(l, Nil:List[String])((i, acc) => Cons(i.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((a, acc) => Cons(f(a), acc))

  def mapWithStackSafe[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    foldLeft(l, buf)((acc,a) => acc += f(a))

    List(buf.toList: _*)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((a, acc) => if(f(a)) Cons(a, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)

  def zipIntegers(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipIntegers(xs, ys))
    }

  def zip[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] =
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zip(xs, ys)(f))
    }

  def zipWithStackSafe[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = {
    val buf = new collection.mutable.ListBuffer[C]

    def go(l: List[A], r: List[B]): Unit = {
      (l, r) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(x, xs), Cons(y, ys)) => buf += f(x,y); go(xs, ys)
      }
    }

    go(l, r)
    List(buf.toList: _*)
  }

  @tailrec
  def startsFrom[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xs), Cons(y, ys)) => if(x == y) startsFrom(xs, ys)  else false
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case _ if startsFrom(l, sub) => true
    case Cons(x, xs) => hasSubsequence(xs, sub)
    case _ => false
  }
}
