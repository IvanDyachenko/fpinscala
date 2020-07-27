package fpinscala.datastructures

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

  /**
    * Exercise 3.9
    * Compute the length of a list using `foldRight`.
    */
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  /**
    * Exercise 3.10
    * Our implementation of `foldRight` is not tail-recursive and will result in a `StackOver-
    * flowError` for large lists (we say it's not `stack-safe`). Convince yourself that this is the
    * case, and then write another general list-recursion function, `foldLeft`, that is
    * tail-recursive, using the techniques we discussed in the previous chapter.
    */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Exercise 3.11
    * Write `sum`, `product`, and a function to compute the length of a list using `foldLeft`.
    */
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  /**
    * Exercise 3.12
    * Write a function that returns the reverse of a list (given `List(1, 2, 3)`) it returns
    * `List(3, 2, 1)`). See if you can write it using a fold.
    */
  def reverse2[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

  /**
    * Exercise 3.13
    * Can you write `foldLeft` in terms of `foldRight`? How about the other way around?
    * Implementing `foldRight` via `foldLeft` is useful because it lets us implement
    * `foldRight` tail-recursively, which means it works even for large lists without
    * overflowing the stack.
    */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight[A, B => B](l, b => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B => B](l, b => b)((g, a) => b => g(f(a, b)))(z)

  /**
    * Exercise 3.14
    * Implement `append` in terms of either `foldLeft` or `foldRight`.
    */
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft[A, List[A] => List[A]](l, xs => xs)((g, x) => xs => g(Cons(x, xs)))(r)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  /**
    * Exercise 3.15
    * Write a function that concatenates a list of lists into a single list. Its runtime
    * should be linear in the total lenght of all lists. Try to use functions we have already
    * defined.
    */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(appendViaFoldRight)

  /**
    * Exercise 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    */
  def increase(l: List[Int], by: Int): List[Int] =
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + by, xs))

  /**
    * Exercise 3.17
    * Write a function that turns each value in a `List[Double]` into a `String`. You can use
    * the expression `d.toString` to convert some `d: Double` to a `String`.
    */
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  /**
    * Exercise 3.18
    * Write a function `map` that generalizes modifying each element in a list while maintain-
    * ing the structure of the list.
    */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  /**
    * Exercise 3.19
    * Write a function `filter` that removes elements from a list unless they satisfy a given
    * predicate. Use it to remove all odd numbers from a `List[Int]`.
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  /**
    * Exercise 3.20
    * Write a function `flatMap` that works like `map` except that the function given will return
    * a list instead of a single result, and that list should be inserted into the final resulting
    * list.
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  /**
    * Exercise 3.21
    * Use `flatMap` to implement `filter`.
    */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  /**
    * Exercise 3.22
    * Write a function that accepts two lists and constructs a new list by adding correspond-
    * ing elements.
    */
  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) | (_, Nil)        => Nil
    case (Cons(a, ak), Cons(b, bk)) => Cons(a + b, addPairwise(ak, bk))
  }

  /**
    * Exercise 3.23
    * Generalize the function you just wrote so that it's not specific to integers or addition.
    */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(a, ak), Cons(b, bk)) => Cons(f(a, b), zipWith(ak, bk)(f))
  }

}
