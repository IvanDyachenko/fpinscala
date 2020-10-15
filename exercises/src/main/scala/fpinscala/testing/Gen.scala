package fpinscala.testing

import fpinscala.state._
import fpinscala.laziness.Stream
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import java.util.concurrent.{Executors,ExecutorService}

import Gen._
import Prop._

/*
 * The library developed in this chapter goes through several
 * iterations. This file is just the shell, which you can fill in and
 * modify while working through the chapter.
 */

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  /**
    * Exercise 8.9
    *
    * Now that we have a representation of `Prop`, implement `&&` and
    * `||` for composing `Prop` values. Notice that in the case of
    * failure we don't know which property was responsible, the left
    * or the right. Can you devise a way of handling this, perhaps by
    * allowing `Prop` values to be assigned a tag or label which gets
    * displayed in the event of a failure?
    */
  def &&(that: Prop): Prop = Prop { (maxSize, testCases, rng) =>
    run(maxSize, testCases, rng) match {
      case Passed => that.run(maxSize, testCases, rng)
      case failed => failed
    }
  }

  def ||(that: Prop): Prop = Prop { (maxSize, testCases, rng) =>
    run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) => that.tag(failure).run(maxSize, testCases, rng)
      case passed                        => passed
    }
  }

  private def tag(msg: String) = Prop { (maxSize, testCases, rng) =>
    run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) => Falsified(s"$msg\n$failure", successes)
      case passed                        => passed
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (_, testCases, rng) =>
    randomStream(gen)(rng).zipWith(Stream.from(0)) { (a, i) =>
      try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.take(testCases).find(_.isFalsified).getOrElse(Passed)
  }

  private def forAll[A](forSize: Int => Gen[A])(f: A => Boolean): Prop = Prop { (maxSize, testCases, rng) =>
    val testCasesPerSize = (testCases + maxSize - 1) / maxSize
    val props = Stream.from(0).take((maxSize min testCases) + 1) // Make one property per size, but no more than testCases properties
      .map(size => forAll(forSize(size))(f))
      .map(prop => Prop { (maxSize, _, rng) => prop.run(maxSize, testCasesPerSize, rng) })
      .toList
    val prop = props.reduce(_ && _)

    prop.run(maxSize, testCases, rng)
  }

  def forAll[A](gen: SGen[A])(f: A => Boolean): Prop = forAll(gen(_))(f)

  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  /**
    * Generates an infinite stream of `A` values be repeatedly
    * sampling a generator.
    */
  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[A,B](f: A => B): Gen[B] = ???

  /**
    * Exercise 8.10
    *
    * Implement helper functions for converting `Gen` to `SGen`. You
    * can add this as a method on `Gen`.
    */
  def unsized: SGen[A] = SGen(_ => this)

  /**
    * Exercise 8.6
    *
    * Implement `flatMap`, and then use it to implement this more
    * dynamic version of `listOfN`. Put `flatMap` and `listOfN` in the
    * `Gen` class.
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOf: SGen[List[A]] =
    Gen.listOf(this)

  def listOf1: SGen[List[A]] =
    Gen.listOf1(this)

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
  //size.flatMap(l => Gen(State.sequence(List.fill(l)(sample))))
    size.flatMap(l => Gen.listOfN(l, this))
}

object Gen {

  /**
    * Exercise 8.5
    *
    * Let's see what else we can implement using this representation
    * of `Gen`. Try implementing `unit`, `boolean`, and `listOfN`.
    */
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
    * Exercise 8.12
    *
    * Implement a `listOf` combinator that doesn't accept an explicit
    * size. It should return an `SGen` insted of a `Gen`. The
    * implementation should generate lists of the requested size.
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  /**
    * Exercise 8.13
    *
    * Define `listOf1` for generating nonempty lists, and then update
    * your specification of `max` to use this generator.
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  /**
    * Exercise 8.4
    *
    * Implement `Gen.choose` using this representation of `Gen`. It
    * should generate integers in the range `start` to
    * `stopExclusive`. Feel free to use functions you've already written.
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val interval = stopExclusive - start
    val state = State(RNG.nonNegativeLessThan(interval)).map(i => start + i)
    Gen(state)
  }

  /**
    * Exercise 8.7
    * 
    * Implement `union`, for combining two generators of the same type
    * into one, by pulling values each generator with equal likelihood.
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  /**
    * Exercise 8.8
    *
    * Implement `weighted`, a version of `union` that accepts a weight
    * for each `Gen` and generates values from each `Gen` with
    * probability proportional to its weight.
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val threshold1 = weight1.abs / (weight1.abs + weight2.abs)

    Gen(State(RNG.double)).flatMap(d => if (d < threshold1) gen1 else gen2)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(size: Int): Gen[A] = forSize(size)

  /**
    * Exercize 8.11
    *
    * Not surprisingly, `SGen` at a minimum supports many of the same
    * operations as `Gen`, and the implementations are rather
    * mechanical. Define some convenience functions on `SGen` that
    * simply delegate to the corresponding functions on `Gen`.
    */
  def map[A,B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { size =>
    forSize(size).flatMap(f(_).forSize(size))
  }
}

object Main extends App {
  /**
    * Exercise 8.14
    *
    * Write a property to verify the behavior of `List.sorted`.
    */
  val listProp = forAll(listOf(Gen.choose(-10, 10))) { ls =>
    val ms = ls.sorted

    ms.isEmpty || ms.tail.isEmpty || (ms zip ms.tail).forall { case (a, b) => a <= b }
  }

  run(listProp)
}
