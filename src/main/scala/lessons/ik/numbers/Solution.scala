package lessons.ik.numbers

object Solution extends App {

  /*
  * https://www.tutorialspoint.com/scala/scala_operators.htm
  *
  * nums % 10 is how you get the last character of a nums
  * nums - (nums % 10) is like subtracting the last character of nums
  * (nums - (nums % 10)) / 10 is how you remove the last character of a number
  *
  * Binary to Int: = total*2 + binaryDigitAtIth
  * Binary to Int: = (total<<1) + binaryDigitAtIth
  * Left shifting by 1 is like multiplying by 2
  * Right shifting by 1 is like dividing by 2
   */

  println(binaryToIntegerBitwise("101"))
  def binaryToIntegerBitwise(n:String) = {
    var sum = 0
    for (i <- 0 until n.size) {
      sum = (sum << 1) | n.charAt(i).asDigit
    }
    sum
  }

  println(binaryToInteger("101"))
  def binaryToInteger(n:String) = {
    var sum = 0
    for (i <- 0 until n.size) {
      sum = (sum * 2) + n.charAt(i).asDigit
    }
    sum
  }

//  alphabet()
  def alphabet():Unit = {
    val word = "abcdefghijklmopqrstuvwxyz"
    word.foreach(e => {
      println(e.asDigit)
    })
  }

//  println(subtractProductAndSum(0))
  def subtractProductAndSum(n: Int): Int = {
    var mul = if (n > 0) 1 else 0
    var sum = 0
    var cur = n

    while (cur > 0) {
      val digit = cur % 10
      mul *= digit
      sum += digit
      cur = (cur - digit)/10
    }

    mul - sum
  }

//  println(getFirstNumber(2345))
  def getFirstNumber(n:Int):Int = {
    var nums = n
    while (nums > 10) {
      nums = (nums - (nums % 10)) / 10
    }
    nums
  }

//  println(countDigits(1000))
  def countDigits(n:Int): Int = {
    var nums = n
    var count = 0
    while (nums > 0) {
      nums = (nums - (nums % 10)) / 10
      count += 1
    }
    count
  }

//  println(findNthDigitOfBaseB(1234, 1, 10))
  def findNthDigitOfBaseB(a:Int, n:Int, b:Int):Integer = {
    var d:Integer = null
    var nums = a
    var count = 0
    while (count < n) {
      d = nums % b
      nums = (nums - d) / b
      count += 1
    }
    d
  }

}
