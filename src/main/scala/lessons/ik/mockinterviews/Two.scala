package lessons.ik.mockinterviews

object Two extends App {

  // Pick the max between forward range
  println(minTaps(8, Array(2,0,0,1,2,0,0,0,4)))
  def minTaps(n:Int, arr:Array[Int]): Int = {
    import collection.mutable.{Map}

    def getMaxIndex(from:Int, to:Int): Int = {
      var maxIndex = from
      for (i <- from to to) {
        // We need to make sure that if bot sprinklers are
        // the same, we choose the one closer to the end for coverage
        maxIndex = if (arr(maxIndex) < arr(i)) i
        else if (arr(maxIndex) == arr(i)) i else maxIndex
      }
      maxIndex
    }

    val map = Map[Int, Int]()
    var position = 0
    while (position < arr.length) {
      // Because there is a chance we'll overflow we need to keep the from/to in ranges
      val left = position - arr(position)
      val right = position + arr(position)
      val from = if (left < 0) 0 else left
      val to = if (right >= arr.length) arr.length-1 else right

      // Get the index of Sprinkler and store it in a map
      val maxIndex = getMaxIndex(from, to)

      // Use a map for consolidation
      // TODO: Backtrack delete
      map.put(maxIndex, arr(maxIndex))

      // Move ahead to the next uncovered range
      if (position != arr.length-1) position = maxIndex + arr(maxIndex) + 1 else position = position + 1
    }

    map.foreach(e => {
      println(s"We need to turn on Sprinkler at position: ${e._1} with value of ${arr(e._1)}")
    })

    // Sprinkler
    if (n < map.size) -1 else map.size
  }

}
