def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ::: List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case (y :: ys) :: zs => y :: flatten(ys) ::: flatten(zs)
  case z :: zs => z :: flatten(zs)
  case Nil => Nil
}

val list = List(2, 3, 5, 7, 11, 13, 17, 19, 23)
val nextList = List(29, 31, 37)

last(list)
init(list)
concat(list, nextList)
reverse(list)
removeAt(2, list)

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

import math.Ordering

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2

    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (_, Nil) => xs
        case (Nil, _) => ys
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n

      merge(msort(fst), msort(snd))
    }
  }
}

val unsorted = List(4, 6, 1,3, 9, 7, 1)

mergesort.msort(unsorted)

val fruits = List("pinapple", "apple", "banana")

mergesort.msort(fruits)

def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => (y * y) :: squareList1(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

val l = 1.to(10).toList

squareList1(l)
squareList2(l)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys =>
    val (fst, rest) = xs span (x => x == y)

    fst :: pack(rest)
}

val l2 = List("a", "a", "a", "b", "c", "c", "a")

pack(l2)

def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x.head, x.length))

encode(l2)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _ )

mapFun(List(1, 2, 3, 4, 5), (x: Int) => x * x)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_, l) => l + 1 )

lengthFun(4 to 13 toList)
