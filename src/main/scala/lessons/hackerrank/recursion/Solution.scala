package lessons.hackerrank.recursion

object Solution extends App {

  // Complete the superDigit function below.
//  println(superDigit("148", 3))
  def superDigit(n: String, k: Int): Int = {
    val sum = n.foldLeft(0l)((agg, e) => agg + e.asDigit) * k + ""

    def superDigit(step:String):Int = {
      if (step.length == 1) {
        step.toInt
      } else {
        superDigit(step.foldLeft(0l)((agg, e) => agg + e.asDigit) + "")
      }
    }

    superDigit(sum)
  }

//  println(powerSum(100,2))
  def powerSum(X: Int, N: Int): Int = {
    var sum = 0

    def permute(i:Int = 1, agg:Double = 0):Unit = {
      if (agg < X) {
        var j = i
        var nodeVal = agg + Math.pow(j, N)
        while (nodeVal <= X) {
          permute(j+1, nodeVal)
          j += 1
          nodeVal = agg + Math.pow(j, N)
        }
      } else if (agg == X) {
        sum += 1
      }
    }

    permute()

    sum
  }

}
