package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25
    * Write a function `size` that counts the number of nodes (leaves and branches) in a tree.
    */
  def size[A](tree: Tree[A]): Int = {
    @annotation.tailrec
    def go(ts: List[Tree[A]], count: Int): Int = ts match {
      case Nil                    => count
      case Cons(Leaf(_), tr)      => go(tr, count + 1)
      case Cons(Branch(l, r), tr) => go(Cons(l, Cons(r, tr)), count + 1)
    }

    go(List(tree), 0)
  }

  /**
    * Exercise 3.26
    * Write a function `maximum` that returns the maximum element in a `Tree[Int]`.
    */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * Exercise 3.27
    * Write a function `depth` that returns the maximum path length form the root of a tree
    * to any leaf.
    */
  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_)             => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  /**
    * Exercise 3.28
    * Write a function `map`, that modifies each element in a tree with a given function.
    */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)             => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
    * Exercise 3.29
    * Generalize `size`, `maximum`, and `map`, writing a new function `fold` that abstracts
    * over their similarities. Reimplement them in terms of this more general function.
    */
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a)             => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(a => a)(_ max _)

  def depthViaFold(tree: Tree[Int]): Int =
    fold(tree)(_ => 0)((dl, dr) => 1 + (dl max dr))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
