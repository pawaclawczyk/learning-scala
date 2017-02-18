def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.0001

  def improve(guess: Double): Double =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(4)

sqrt(0.1e-20)
sqrt(1e60)

5
+ 5

(5
  + 5)

5 +
5

def gcd(a: Int, b: Int): Int =
  if (b == 0) a
  else gcd(b, a % b)

gcd(14, 21)

def recursive_factorial(n: Int): Int =
  if (n == 0) 1
  else n * factorial(n - 1)

recursive_factorial(5)

def factorial(n: Int): Int = {
  def factorialIter(n: Int, acc: Int): Int =
    if (n == 0) acc
    else factorialIter(n - 1, acc * n)

  factorialIter(n - 1, n)
}

factorial(5)
