package fpinscala.state


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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def boolean: Rand[Boolean] = map(int)(_ % 2 == 0)

  /**
    * Exercise 6.1
    *
    * Write a function that uses `RNG.nextInt` to generate a random
    * integer between 0 and `Int.MaxValue` (inclusive).
    *
    * Make sure to handle the corner case when `nextInt` returns
    * `Int.MinValue`, which doesn't have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
    * Exercise 6.2
    *
    * Write a function to generate a `Double` between 0 and 1, not
    * including 1.
    *
    * Note: You can use `Int.MaxValue` to obtain the maximum positive
    * integer value, and you can use `x.toDouble` to convert an `x:
    * Int` to a `Double`.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  /**
    * Exercise 6.3
    *
    * Write functions to generate an `(Int, Double)` pair, a `(Double,
    * Int)` pair, and a `(Double, Double, Double)` 3-tuple. You should
    * be able to reuse the functions you've already written.
    */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    (i, d) -> r2
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    (d, i) -> r
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  /**
    * Exercise 6.4
    *
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldLeft(List.empty[Int] -> rng) { case ((is, rng), _) =>
      val (i, r) = rng.nextInt
      (i :: is) -> r
    }

  /**
    * Exercise 6.5
    *
    * Use `map` to reimplement `double` in a more elegant way.
    */
  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
    * Exercise 6.6
    *
    * Write the implementation of `map2` based on the following
    * signature.
    *
    * This function takes two actions, `ra` and `rb`, and a function
    * `f` for combining their results, and returns a new action that
    * combines them.
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  /**
    * Exercise 6.7
    *
    * Implement `sequence` for combining a `List` of transitions into
    * a single transition.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  /**
    * Exercise 6.8
    *
    * Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  /**
    * Exercise 6.9
    *
    * Reimplement `map` and `map2` in terms of `flatMap`.
    *
    * The fact that this is possible is what we are referring to when
    * we say that `flatMap` is more powerful than `map` and `map2`.
    */
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def lessThan(n: Int): Rand[Int] =
    flatMap(int) { i =>
      val mod = i % n
      if (i - mod + (n - 1) < 0) lessThan(n) else unit(mod)
    }
}

/**
  * Exercise 6.10
  *
  * Generalize the functions `unit`, `map`, `map2`, `flatMap`, and
  * `sequence`.
  *
  * Add them as methods on the `State` case class where
  * possible. Otherwise you should put them in a `State` companion
  * object.
  */
case class State[S,+A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s1 =>
    val (a, s2) = run(s1)
    f(a).run(s2)
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.reverse.foldLeft(unit[S, List[A]](Nil))((as, a) => a.map2(as)(_ :: _))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
