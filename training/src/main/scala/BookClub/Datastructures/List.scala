package BookClub.Datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(y, ys) => ys
  }

  def setHead[A](head: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(head, Nil)
    case Cons(y, ys) => Cons(head, ys)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (xs, 0) => xs
    case (Cons(x, xs), m) => drop(xs, m - 1)
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = Nil

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  def init[A](l: List[A]): List[A] = Nil

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldProduct(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def leftSum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def leftProduct(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def leftLength[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((tail, head) => Cons(head, tail))

  def leftAsRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def rightAsLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  def flatConcat[A](l: List[List[A]]): List[A] = foldLeft(l, )

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
