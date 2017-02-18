package rational

class Rational(x: Int, y: Int) {

  def gcd(a: Int, b: Int): Int = if (0 == b) a else gcd(b, a % b)
  val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def this(a: Int) = this(a, 1)

  def +(other: Rational): Rational = {
    new Rational(
      this.numer * other.denom + other.numer * this.denom,
      this.denom * other.denom
    )
  }

  def unary_- :Rational = new Rational(- this.numer, this.denom)

  def -(other: Rational): Rational = this + (- other)

  def *(that: Rational): Rational = new Rational(this.numer * that.numer, this.denom * that.denom)

  def < (that: Rational): Boolean = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational = if (this < that) that else this

  override def toString: String = if (1 == denom) numer.toString else numer + "/" + denom
}
