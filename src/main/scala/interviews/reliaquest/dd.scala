package interviews.reliaquest

object Solution extends App {

  // 122
  println(((1 * 10) + 2) * 10 + 2)
//  println(calculate("6-7+(1-1)"))
//  println(calculate(""))
  // '+', '-', '*', '/'
//  println(getSubExpression("((1)+(1))"))
  def getSubExpression(str: String, start: Int = 0): (String, Int) = {
    import collection.mutable.Buffer

    if (str.charAt(start) != '(') return ("", 0)

    var count = 1
    var i = start + 1
    val buffer = Buffer[Char]()
    while (i < str.length && count != 0) {
      val c = str.charAt(i)
      if (c == '(') count += 1
      else if (c == ')') count -= 1
      buffer.append(c)
      i += 1
    }

    buffer.remove(buffer.length - 1)
    (buffer.mkString, i-1)
  }

  def calculate(str: String): Int = {
    if (str.length == 0) return 0

    var total = 0

    var curTotal = 0
    var lastOperator = '+'
    var i = 0
    while (i < str.length) {
      str.charAt(i) match {
        case '+' => {
          if (lastOperator == '+') total += curTotal
          else total -= curTotal
          lastOperator = '+'
          curTotal = 0
        }
        case '-' => {
          if (lastOperator == '+') total += curTotal
          else total -= curTotal
          lastOperator = '-'
          curTotal = 0
        }
        case '(' => {
          val ex = getSubExpression(str, i)
          val innerTotal = curTotal + calculate(ex._1)

          if (lastOperator == '+') total += innerTotal
          else total -= innerTotal

          i = ex._2
        }
        case e: Char => {
          curTotal = (curTotal * 10) + e.asDigit
        }
      }
      i += 1
    }

    if (lastOperator == '+') total += curTotal
    else total -= curTotal

    total
  }
}
