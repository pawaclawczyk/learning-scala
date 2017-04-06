def fibonacci(n: Int): Int = {
  def fibInc(i: Int, f: Int, s: Int): Int = {
    if (i == n) s
    else fibInc(i + 1, s, f + s)
  }

  if (0 == n) 0
  else fibInc(1, 0, 1)
}

fibonacci(0)
fibonacci(1)
fibonacci(2)
fibonacci(3)
fibonacci(4)
fibonacci(5)
fibonacci(10)
fibonacci(43)

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def go (i: Int, res: Boolean): Boolean = {
    if (!res) res
    else if (i == as.length - 2) res
    else go(i + 1, ordered(as(i), as(i + 1)))
  }

  go(0, true)
}

val sorted = Array(1, 3, 4, 6, 7, 9, 11)
val reverseSorted = sorted.toList.reverse.toArray
val unsorted = Array(5,  2, 8, 3, 7, 10)

isSorted(sorted, (x: Int, y: Int) => if (x <= y) true else false )
isSorted(reverseSorted, (x: Int, y: Int) => if (x >= y) true else false )
isSorted(unsorted, (x: Int, y: Int) => if (x <= y) true else false )
isSorted(unsorted, (x: Int, y: Int) => if (x >= y) true else false )

def curry[A, B, C](f: (A, B) => C): A => B => C = {
  (a: A) => (b: B) => f(a, b)
}

val curriedSum = curry((a: Int, b: Int) => a + b)

val add5 = curriedSum(5)

add5(3)
add5(10)

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

val uncurriedSum = uncurry(curriedSum)

uncurriedSum(3, 4)

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
}

val curriedMultiply = curry((x: Int, y: Int) => x * y)

val add7 = curriedSum(7)
val multiplyBy3 = curriedMultiply(3)

multiplyBy3(3)
multiplyBy3(7)

val plus7times3 = compose(multiplyBy3, add7)
val times3plus7 = compose(add7, multiplyBy3)

plus7times3(2)
times3plus7(2)

