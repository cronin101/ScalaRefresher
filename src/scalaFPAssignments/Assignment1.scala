package scalaFPAssignments

object Assignment1 {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        print(pascal(col, row) + " ")
      }
      println
    }
    println("Parens Balance Check")
    def checkString(s: String) = {
      print("With " + s + ": ")
      println(parensBalanced(s.toList))
    }
    checkString("(())()(")
    checkString("(())")
  }

  /** Returns the element at Column c and Row r of Pascal's Triangle */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /** Returns whether a string has balanced parentheses */
  def parensBalanced(chars: List[Char]): Boolean = {
    def recurBalanced(depth: Int, remaining: List[Char]): Boolean = {
      remaining match {
        case Nil => depth == 0
        case '(' :: xs => recurBalanced(depth + 1, xs)
        case ')' :: xs => if (depth > 0)
                            recurBalanced(depth - 1, xs)
                          else
                            false
        case _ :: xs   => recurBalanced(depth, xs)
      }
    }
    recurBalanced(0, chars)
  }

  /** Returns how many ways you can produce an amount of money using a given set of coins */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recurCount(total: Int, coinsLeft: List[Int]): Int = {
      val remaining = money - total
      if (remaining == 0 || coinsLeft.isEmpty || remaining < coinsLeft.head)
        0
      else if (total + coinsLeft.head == money)
        1
      else
        recurCount(total + coinsLeft.head, coinsLeft) +
            recurCount(total, coinsLeft.tail)
    }
    recurCount(0, coins.sortWith(_ < _).distinct)
  }
}