package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c , r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countOpen(chars: List[Char], open: Int): Int =
      if (chars.isEmpty) open
      else if (0 > open) open
      else if ('(' == chars.head) countOpen(chars.tail, open + 1)
      else if (')' == chars.head) countOpen(chars.tail, open - 1)
      else countOpen(chars.tail, open)

    0 == countOpen(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int =
      if (0 == money) 1
      else if (0 > money) 0
      else if (coins.isEmpty) 0
      else loop(money - coins.head, coins) + loop(money, coins.tail)

    loop(money, coins)
  }
}
