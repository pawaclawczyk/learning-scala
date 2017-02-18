def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x

def sumRec(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sumRec(f, a + 1, b)

sumRec(id, 1, 10)
sumRec(cube, 1, 10)

sumRec(x => x * x, 1, 10)

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }

  loop(a, 0)
}

sum(cube)(1, 10)

def product(f: Int => Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) * acc)
  }

  loop(a, 1)
}

def factorial(n: Int): Int = {
  product((x: Int) => x)(1, n)
}

factorial(5)

def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, zero: Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, reduce(acc, map(a)))
  }

  loop(a, zero)
}

def sigma(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x + y, 0)(a, b)

sigma(cube)(1, 10)


import math.abs

def fixedPoint(f: Double => Double, x: Double): Double = {
  def isGoodEnough(guess: Double) = {
    abs(guess - f(guess)) / guess < 0.001
  }

  def improve(guess: Double) = {
    def average(f: Double => Double, x: Double): Double = {
      (x + f(x)) / 2
    }

    average(f, guess)
  }

  def loop(guess: Double): Double = {
    if (isGoodEnough(guess)) guess
    else loop(improve(guess))
  }

  loop(1.0)
}

def sqrt(x: Double) = fixedPoint(y => x / y, x)

sqrt(4)
sqrt(2.0)

import rational.Rational

val x = new Rational(1, 2)
val y = new Rational(2, 4)

x + y
-x
x - y
x * y

new Rational(2, 3) max new Rational(1, 2)

new Rational(42)
