package recfun
import scala.collection.mutable.{ListBuffer, Map}

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(countChange(5,List(1,2,3)))
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

      var charList = moveForward(chars)

      def checkBal(isBal: Boolean):Boolean =
      {
        var currentlyBal = true

        if(charList.head == '(')
        {
          currentlyBal = false
          charList = moveForward(charList.tail)
          currentlyBal  = checkBal(false)
        }
        if(charList.head == ')' && !isBal)
        {
          charList = moveForward(charList.tail)
          true
        }
        else if(charList.head == ')' && isBal) false
        else if(charList.head == '-') currentlyBal && isBal
        else
        {
          charList = moveForward(charList)
          checkBal(isBal)
        }
      }

      checkBal(true)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def repeat(lastMaxCoin_total_coll: List[(Int, Int)], count: Int): Int = {

      if (lastMaxCoin_total_coll.isEmpty)
        count
      else
      {

        val coinTable = ListBuffer[(Int, Int)]()
        var newCount = count

        for ((lastMaxCoin, total) <- lastMaxCoin_total_coll)
        {
          if (total < money)
          {
            for (c <- coins)
            {
              if (c >= lastMaxCoin)
              {
                val e = (c, total + c)
                coinTable += e
              }
            }
          } else if (total == money)
            newCount += 1
        }
        repeat(coinTable.toList, newCount)
      }
    }

    val b = coins.map { c => (c, c) }
    repeat(b, 0)
  }
}
