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
    if(r==c || c==0) 1
    else if(c>r) 0
    else pascal(c-1,r-1) + pascal(c,r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

    def para(a:Int,b :Int, chars: List[Char]): Int = {
      if(a<b) -1
      else if(chars.isEmpty) 0
      else if(chars.head == '(') 1+para(a+1,b,chars.tail)
      else if(chars.head == ')') -1+para(a,b+1,chars.tail)
      else para(a,b,chars.tail)
    }
    val sum:Int = para(0,0,chars)
    if (sum==0) true else false
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(coins.isEmpty) 0
    else if(money < 0) 0
    else countChange(money,coins.tail) +  countChange(money - coins.head, coins)
  }
  }
