val s = 1 to 10 toSet

s map (_ / 2)

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens

    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  placeQueens(n)
}

queens(4)

def show(queens: List[Int]) = {
  val lines = for (col <- queens.reverse)
    yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString

  "\n" + (lines mkString "\n")
}

queens(4) map show

val RomanNumeral = Map("I" -> 1, "V" -> 5, "X" -> 10)

RomanNumeral get "C"

class Poly(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))


  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(0 -> 1, 1 -> 2)
val p2 = new Poly(1 -> 3, 2 -> 4)

p1 + p2

Map("A" -> "a", "B" -> "c") + ("B" -> "b")
