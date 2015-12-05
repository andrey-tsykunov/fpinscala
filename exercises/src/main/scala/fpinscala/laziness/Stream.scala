package fpinscala.laziness

import Stream._
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

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(a, t) => if (n > 0) t().drop(n - 1) else this
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(a, t) if p(a()) => cons[A](a(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty:Stream[A]) {
    (a, st) => {
      if (p(a)) cons(a, st) else empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}