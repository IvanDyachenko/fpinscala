package fpinscala.datastructures

import java.lang.reflect.Constructor

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

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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

  /**
    * Exercise 3.2
    * Implement the function `tail` for removing the first element of a `List`. Note that the
    * function takes constant time.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, t) => t
  }

  /**
    * Exercise 3.3
    * Using the same idea, implement the function `setHead` for replacing the first element
    * of a `List` with a different value.
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil        => throw new UnsupportedOperationException("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 3.4
    * Generalize `tail` to the fuction `drop`, which removes the first `n` elements from a list.
    * Note that this function takes time proportional only to the number of elements being
    * dropped - we don't need to make a copy of the entire `List`.
    */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  /**
    * Exercise 3.5
    * Implement `dropWhile`, which removes elements from the `List` prefix as long as they
    * match a predicate.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(l: List[A], m: List[A] = Nil): List[A] = l match {
      case Nil => m
      case Cons(h, t) => go(t, Cons(h, m))
    }

    go(l)
  }

  /**
    * Exercise 3.6
    * Implement a function `init` that returns a `List` consisting of all but the last element
    * of a `List`.
    */
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(l: List[A], m: List[A] = Nil): List[A] = l match {
      case Nil          => throw new UnsupportedOperationException("init of empty list")
      case Cons(_, Nil) => m
      case Cons(h, t)   => go(t, Cons(h, m))
    }

    reverse(go(l))
  }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
