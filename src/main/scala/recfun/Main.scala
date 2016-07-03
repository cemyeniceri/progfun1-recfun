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
  def pascal(col:Int, row:Int):Int = {
    def pascalRec(col:Int, row:Int): Int = {
      if(col==0 || row==col) 1 else pascalRec(col-1, row-1) + pascalRec(col, row-1)
    }
    pascalRec(col,row)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(chars: List[Char], openedNumber: Int, closedNumber: Int): Boolean={
      if(chars.isEmpty){
        openedNumber == closedNumber
      }else{

        val newOpenedNumber = if(chars.head=='(') openedNumber + 1 else openedNumber
        val newClosedNumber = if(chars.head==')') closedNumber + 1 else closedNumber

        if (newClosedNumber > newOpenedNumber)
          false
        else
          balanceRec(chars.tail, newOpenedNumber, newClosedNumber)
      }
    }
    balanceRec(chars, 0, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
