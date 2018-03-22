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

    def getRow(r: Int):Seq[Int] = {
      if (r==0) { Seq(1) }
      else if (r==1) { Seq(1, 1)}
      else {
        val prevRow = getRow(r-1)
        buildRow(prevRow)
      }
    }

    def buildRow(prevRow: Seq[Int]):Seq[Int] = {
      var newRow = Seq(1)
      var x = 1
      while (x < prevRow.length) {
        newRow = newRow :+ (prevRow(x-1) + prevRow(x))
        x = x + 1
      }
      newRow = newRow :+ 1
      newRow
    }

    val relevantRow = getRow(r)
    relevantRow(c)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isBalanced(chars: List[Char], openParens: Int): Boolean = {
      if (chars.isEmpty)
        openParens == 0
      else if (chars.head == '(')
        isBalanced(chars.tail, openParens + 1)
      else if (chars.head == ')')
        openParens > 0 && isBalanced(chars.tail, openParens - 1)
      else
        isBalanced(chars.tail, openParens)
    }

    isBalanced(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
