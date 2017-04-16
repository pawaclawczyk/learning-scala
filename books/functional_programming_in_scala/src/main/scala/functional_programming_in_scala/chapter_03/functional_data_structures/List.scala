package functional_programming_in_scala.chapter_03.functional_data_structures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def prod(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * prod(xs)
  }
}
