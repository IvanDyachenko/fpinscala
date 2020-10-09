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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[A,B](f: A => B): Gen[B] = ???

  /**
    * Exercise 8.6
    *
    * Implement `flatMap`, and then use it to implement this more
    * dynamic version of `listOfN`. Put `flatMap` and `listOfN` in the
    * `Gen` class.
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

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

trait SGen[+A] {

}

