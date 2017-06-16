package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(()(())".toList))
    
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
    {


      def moveForward(char: List[Char]): List[Char] =
      {
        if (char.isEmpty)
          List('-')
        else
          if (char.head == '(' || char.head == ')') char else moveForward(char.tail)
      }

      var tempChar = moveForward(chars)

      def checkBalance(): Boolean =
      {

        var isBalanced = true
        if(tempChar.head == '(') { isBalanced = false; tempChar = moveForward(tempChar.tail) }
        if(tempChar.head == '(' && !isBalanced) checkBalance()
        if(tempChar.head == ')' && isBalanced) return false
        if (tempChar.head == ')' && !isBalanced)
        {
          isBalanced = true
          tempChar = moveForward(tempChar.tail)
        }
        if(tempChar.head == '-')
          isBalanced
        else
        {
          tempChar = moveForward(tempChar)
          checkBalance()
        }
      }
      checkBalance()
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
