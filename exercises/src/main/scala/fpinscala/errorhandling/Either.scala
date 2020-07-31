package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

/**
  * Exercise 4.6
  * Implement versions of `map`, `flatMap`, `orElse`, and `map2` on `Either` that operate in the
  * `Right` value.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
 }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { ra <- this; rb <- b } yield f(ra, rb) //this.flatMap(a => b.map(f(a, _)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  /**
    * Exercise 4.7
    * Implement `sequence` and `traverse` for `Either`. These should return the first error
    * that's encountered, if there is one.
    */

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
//  as match {
//    case Nil     => Right(Nil)
//    case h :: tl => (f(h) map2 traverse(tl)(f))(_ :: _)
//  }
    as
      .foldLeft[Either[E, List[B]]](Right(Nil)) { (xs, a) =>
        xs.map2[E, B, List[B]](f(a))((ys, y) => y :: ys)
      }
      .map(_.reverse)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
//  es match {
//    case Nil => Right(Nil)
//    case h :: tl => (h map2 sequence(tl))(_ :: _)
//  }
    es.foldLeft[Either[E, List[A]]](Right(Nil))((xs, e) => (e map2 xs)(_ :: _)).map(_.reverse)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
