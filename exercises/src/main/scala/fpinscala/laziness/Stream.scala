package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /**
    * Exercise 5.1
    * Write a function to convert a `Stream` to a `List`, which will force its evaluation and let
    * you look at it in the REPL. You can convert to the regular `List` type in the standard
    * library. You can place this and other functions that operate on a `Stream` inside the
    * `Stream` trait.
    */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] =
      s match {
	case Empty => Nil
        case Cons(h, t) => go(t(), h() :: l)
      }

    go(this, List.empty[A]).reverse
  }

  /**
    * Exercise 5.2
    * Write the function `take(n)` for returning the first `n` elements of a `Stream`.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case _                    => empty
  }

  /**
    * Exercise 5.2
    * Write the function `drop` fo skipping the first `n` elements of a `Stream`.
    */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  /**
    * Exercise 5.3
    * Write the function `takeWhile` for returning all starting elements of a `Stream` that
    * match the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _                    => empty
  }

  /**
    * Exercise 5.4
    * Implement `forAll`, which checks that all elements in the `Stream` match a given predi-
    * cate. Your implementations should terminate the traversal as soon as it encounters a
    * nonmatching value.
    */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
    * Exercise 5.5
    * Use `foldRight` to implement `takeWhile`.
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) {
      case (a, s) if p(a) => cons(a, s)
      case (_, s)         => s
    }

  /**
    * Exercise 5.6
    * Implement `headOption` using `foldRight`.
    */
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  /**
    * Exercise 5.7
    * Implement `map`, `filter`, `append`, and `flatMap` using `foldRight`. The `append` method
    * should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, s) => cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) {
      case (a, s) if p(a) => cons(a, s)
      case (_, s)         => s
    }

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???
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
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
