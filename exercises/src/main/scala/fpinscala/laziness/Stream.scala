package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
    this match {
      case Cons(a,t) => t().foldLeft(f(z, a()))(f)
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, acc) => p(a) || acc) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = foldRight(Nil:List[A])((a, ls) => a :: ls)

/*  def take(n: Int): Stream[A] = foldRight((empty:Stream[A], 0)) {
    case (a, (st, i)) => if(i > n) (st, i) else (cons(a, st), i + 1)
  }._1*/

  def take(n: Int): Stream[A] = this match {
    case Cons(a, t) if n > 0 => cons[A](a(), t().take(n - 1))
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (i, Cons(a, t)) if i > 0 => Some((a(), (i - 1, t())))
    case _ => None
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(a, t) => if (n > 0) t().drop(n - 1) else this
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(a, t) if p(a()) => cons[A](a(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(a, t) if p(a()) => Some((a(), t()))
    case _ => None
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty:Stream[A]) {
    (a, st) => {
      if (p(a)) cons(a, st) else empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, t) => p(a) && t)

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  def map[B](f: A => B): Stream[B] =  foldRight(empty[B])((a, t) => cons(f(a), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =  unfold(this) {
    case Cons(a, t) => Some((f(a()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] =  foldRight(empty[A])(
    (a, t) => if(f(a)) cons(a, t) else t
  )

  def append[B >: A](b: => Stream[B]): Stream[B] =  foldRight(b)(
    (b, t) => cons(b, t)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] =  foldRight(empty[B])(
    (a, t) => f(a) append t
  )

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Cons(x, xs), Empty) => {
      val zipped = (Some(x()), None)
      val next = (xs(), Empty)

      Some((zipped, next))
    }
    case (Empty, Cons(y, ys)) => {
      val zipped = (None, Some(y()))
      val next = (Empty, ys())

      Some((zipped, next))
    }
    case (Empty, Empty) => None
    case (Cons(x, xs), Cons(y, ys)) => {
      val zipped = (Some(x()), Some(y()))
      val next = (xs(), ys())

      Some((zipped, next))
    }
  }

  def zip[B](s: Stream[B]): Stream[(A, B)] = unfold((this, s)) {
    case (_, Empty) => None
    case (Empty, _) => None
    case (Cons(x, xs), Cons(y, ys)) => {
      val zipped = (x(), y())
      val next = (xs(), ys())

      Some((zipped, next))
    }
  }

  /*
  * similar to List
  * */
  def startsFrom[A](s: Stream[A]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Empty, _) => false
    case (Cons(x, xs), Cons(y, ys)) => if(x() == y()) xs().startsFrom(ys()) else false
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, xs) => Some(s, xs())
    case Empty => None
  } append Stream(Empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))) {
    (a, acc) => {
      lazy val accX = acc;
      val b = f(a, accX._1)
      (b, cons(b, accX._2))
    }
  }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {

    def generate(x: Int, y: Int): Stream[Int] = cons(x, generate(y, x + y))

    generate(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def generate(s: S): Stream[A] = f(s) match {
      case Some((a, s1)) => cons(a, generate(s1))
      case None => empty[A]
    }

    generate(z)
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)) {
    case (x, y) => Some((x, (y, x + y)))
  }

  def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))
}