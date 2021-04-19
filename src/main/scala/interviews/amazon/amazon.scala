package interviews.amazon

object amazon extends App {

  def songsList(a: Array[Int]): Unit = {

    for (i <- 0 until a.length)
      for (j <- (i + 1) until a.length)
        if (i != j) {

        }
  }

  def findMinComplexity(complexity: Array[Int], days: Int): Int = {
    val pairs = complexity.length - days + 1

    var pairMax = 0
    var remainMax = 0
    var minComplexity = complexity.sum

    // O(n^2) because we're iterating through the list for each daily sequence
    for (i <- 0 to complexity.length - pairs) {
      val rangeLimit = i + pairs
      for (j <- 0 to complexity.length - 1) {
        val currentComplexity = complexity(j)
        if (j >= i && j < rangeLimit) {
          if (pairMax < currentComplexity) pairMax = currentComplexity
        } else remainMax += currentComplexity
      }

      val currentComplexitySum = pairMax + remainMax
      if (minComplexity > currentComplexitySum) minComplexity = currentComplexitySum

      pairMax = 0
      remainMax = 0
    }

    minComplexity
  }

  println(findMinComplexity(Array(10, 10, 10, 10, 10), 4))

}
