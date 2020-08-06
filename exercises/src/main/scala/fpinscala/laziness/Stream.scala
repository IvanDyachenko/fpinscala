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
    def go(as: Stream[A], ls: List[A]): List[A] =
      as match {
        case Cons(h, t) => go(t(), h() :: ls)
        case _          => ls
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

  /**
    * Exercise 5.13
    * Use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith`, and `zipAll`.
    * The `zipAll` function should continue the traversal as long as either stream
    * has more elements - it uses `Option` to indicate whether each stream has been
    * exhausted.
    */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold[B, Stream[A]](this) {
      case Empty       => None
      case Cons(a, as) => Some((f(a()), as()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (n, Cons(a, as)) if n > 0 => Some((a(), (n - 1, as())))
      case _                         => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _                          => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some(((Some(a()), Some(b())), (as(), bs())))
      case (Cons(a, as), _)           => Some(((Some(a()), None), (as(), empty)))
      case (_, Cons(b, bs))           => Some(((None, Some(b())), (empty, bs())))
      case _                          => None
    }

  /**
    * Exercise 5.14
    * Implement `startsWith` using functions you've written. It should check if one
    * `Stream` is a prefix of another. For instance, `Stream(1, 2, 3) startsWith Stream(1, 2)`
    * would be `true`.
    */
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (x, y) => x == y
    }
//  !zipAll(s).exists {
//    case (None, Some(_))  => true
//    case (x, y) if x != y => true
//    case _                => false
//  }

  /**
    * Exercise 5.15
    * Implement `tails` using `unfold`. For a given `Stream`, `tails` returns the `Stream` of suf-
    * fixes of the input sequence, starting with the original `Stream`. For example, given
    * `Stream(1, 2, 3)`, it would return `Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())`.
    */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) => Some(s -> t())
      case _              => None
    } append Stream(empty)

  /**
    * We can now implement `hasSubsequence` using functions we've written
    */
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))

  /**
    * Exercise 5.16
    * Generalize `tails` to the function `scanRight`, which is like a `foldRight` that
    * returns a stream of the intermediate results.
    */
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((Stream(z), z)) { (a, state) =>
      lazy val (s, p) = state
      val c = f(a, p)
      (cons(c, s), c)
    }._1
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

  /**
    * Exercise 5.8
    * Generalize `ones` slightly to the function `constant`, which returns an infinite `Stream` of
    * a given value.
    */
  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = Cons(() => a, () => s)
    s
  }

  /**
    * Exercise 5.9
    * Write a function that generates an infinite stream of integers, starting from `n`, then `n + 1`,
    * `n + 2`, and so on.
    */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
    * Exercise 5.10
    * Write a function `fibs` that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
    * 2, 3, 5, 8, and so on.
    */
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  /**
    * Exercise 5.11
    * Write a more general stream-building function called `unfold`. It takes an initial state,
    * and a function for producing both the next state and the next value in the generated
    * stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None          => empty
    case Some((a, nz)) => cons(a, unfold(nz)(f))
  }

  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]){ case (a, s) => cons(a, unfoldViaFold(s)(f)) }

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) => cons(a, unfoldViaMap(s)(f)) }.getOrElse(empty)

  /**
    * Exercise 5.12
    * Write `fibs`, `from`, and `constant` in terms of `unfold`.
    */
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](c: A): Stream[A] =
    unfold(c)(_ => Some((c, c)))
}
