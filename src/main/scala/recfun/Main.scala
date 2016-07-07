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
    def pascal(c: Int, r: Int): Int = {

      def fact(n: Int): Int = if (n == 1 || n == 0) 1 else n * fact(n - 1)

      if (r == 0) 1
      else fact(r) / (fact(c) * fact(r - c))
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def countBalance(counter: Int, str: List[Char]): Boolean = {
        if (str.isEmpty) counter == 0
        else str.head match {
          case '(' => countBalance(counter + 1, str.tail)
          case ')' => if (counter == 0) false else countBalance(counter - 1, str.tail)
          case _ => countBalance(counter, str.tail)
        }
      }
      countBalance(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if(money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

  }
