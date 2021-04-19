package lessons.ik.dp

object Solution extends App {
  println(equalSubSetSumPartition(Array(1,0,-1)).mkString(","))
  def equalSubSetSumPartition(s: Array[Int]): Array[Boolean] = {
    import collection.mutable.Map

    val totalSum = s.sum
    val output = Array.fill[Boolean](s.length)(true)
    val cache = Map[Int, Boolean]()

    // Subset problems!
    // 1. C(n, k)
    // 2. Memoization

    def permute(i:Int = 0, sum:Int = 0, c:Int = 0): Boolean = {
      // Don't choose the whole array
      if (c == s.length-1) return false

      val lsum = s(i) + sum
      val rsum = totalSum - lsum

      // We memoize the left side and cache the right side
      if (cache.contains(lsum)) return cache(lsum)

      var found = false

      if (lsum == rsum) {
        output(i) = false
        return true
      } else {
        var j = i + 1
        while (j < s.length && !found) {
          output(j) = false
          found = permute(j, lsum, c + 1)
          if (!found) output(j) = true
          j += 1
        }
      }

      cache.put(lsum, found)
      output(i) = !found
      found
    }

    if (permute()) output else Array()
  }

//  println(canPartition(Array(3,1,0,-1)).mkString(","))
  def canPartition(s: Array[Int]): Array[Boolean] = {
    import collection.mutable.Map

    val sum = s.sum

    // Integer addition will never be equal to float
    // thus odd sums won't work
    if (sum % 2 != 0) return Array()

    // This only works because we don't have negative numbers
    val half = sum / 2
    val output = Array.fill[Boolean](s.length)(true)
    val cache = Map[Int, Boolean]()

    // Subset problems!
    // 1. C(n, k)
    // 2. Memoization

    def permute(i:Int = 0, sum:Int = 0): Boolean = {
      val lsum = s(i) + sum

      if (cache.contains(lsum)) return cache(lsum)

      var found = false

      if (lsum == half) {
        output(i) = false
        return true
      } else if (lsum < half) {
        var j = i + 1
        while (j < s.length && !found) {
          output(j) = false
          found = permute(j, lsum)
          if (!found) output(j) = true
          j += 1
        }
      }

      cache.put(lsum, found)
      found
    }

    if (permute()) output else Array()
  }

  println(wordBreakCount(Array("k","i","oo"), "kio"))
  def wordBreakCount(dictionary: Array[String], txt: String): Int = {
    import collection.mutable.Map

    if (txt.isEmpty) return 0

    val cache = Map[String, Int]()

    def permute(subTxt:String, i:Int = 0):Integer = {
      if (cache.contains(subTxt)) return cache(subTxt)

      if (i >= txt.length) return 1

      var subCount = 0
      for (word <- dictionary) {
        if (subTxt.startsWith(word)) {
          val frag = subTxt.substring(word.length())
          val count = permute(frag, i + word.length())
          subCount = subCount + count
        }
      }

      cache.put(subTxt, subCount)
      subCount
    }

    permute(txt) % 1000000007
  }

  println(wordBreakII("abc", List("a","bc")).mkString("\n"))
  def wordBreakII(txt:String, dic:List[String]): List[String] = {
    import collection.mutable.{Map, Buffer}

    // We need to recursively call and
    // Then use memoization based on index

    // IMPORTANT!
    if (txt.isEmpty) return List.empty

    val cache = Map[String, Buffer[String]]()

    def permute(subTxt:String): Buffer[String] = {
      if (cache.contains(subTxt)) return cache(subTxt)

      // This is important as it signifies we're at the end of the tree
      if (subTxt.isEmpty) return Buffer("")

      val buf = Buffer[String]()
      for (word <- dic) {
        if (subTxt.startsWith(word)) {
          val subWords = permute(subTxt.substring(word.length))
          // Buffer turns empty because subText.isEmpty wasn't empty but
          // we couldn't find anything in the dictionary for this sub-problem
          subWords.foreach(solution => {
            val sol = if (solution.isEmpty) "" else " " + solution
            buf.append(word + sol)
          })
        }
      }

      // This works because subTxt is the entire substring not partial
      // Therefore not 2 keys will overlap due to subString length differences
      cache.put(subTxt, buf)
      buf
    }

    permute(txt).toList
  }

//  println(wordBreakI("abcd", List("cat", "cats", "and", "sand", "dog")))
  def wordBreakI(txt:String, dic:List[String]): Boolean = {
    import collection.mutable.Map

    // We're just using the the dictionary as options
    // And you just do substrings on each recursion

    var found = false
    val cache = Map[String, Boolean]()

    def permute(i:Int = 0): Unit = {
      val key = txt.substring(i, txt.length)

      if (cache.contains(key)) {
        found = cache(key)
      } else {
        if (i >= txt.length) {
          found = true
          cache.put(key, true)
        } else if (found == false) {
          dic.foreach(e => {
            val l = i+e.length
            if (found == false && l <= txt.length && txt.substring(i, l).equals(e)) {
              permute(l)
            }
          })
          if (!found) {
            cache.put(key, false)
          }
        }
      }
    }

    permute()
    found
  }

//  println(coinChange(Array(86,419,83,408), 6249))
  def coinChange(coins: Array[Int], amount: Int): Int = {
    import collection.mutable.Map

    val cache = Map[Int, Int]()

    // Bottom up approach
    // Put all base cases in cache
    // Every base case equals 1 coin
    coins.foreach(c => cache.put(c, 1))

    // Iterate from 1 to amount and keep track of minimum coins up to i
    for (i <- 1 to amount) {
      if (cache.contains(i)) {
        // Already computed minimum coins up to this i
      } else {
        var min: Integer = null
        // Loop through all coins
        for (j <- 0 until coins.length) {
          val min2 = i - coins(j)
          // Check if i - coin is positive
          // make sure we're going back to a cache that's not -1
          if (min2 > -1 && cache(min2) != -1) {
            // The first coin we pick is the minimum
            // After, we compare and choose the minimum
            min = if (min == null) cache(min2) else Math.min(min, cache(min2))
          }
          // null means there is no coin
          // -1 means no amount of change found
          cache.put(i, if (min != null && min != -1) min + 1 else -1)
        }
      }
    }

    // Check if amount exists
    if (cache.contains(amount)) cache(amount) else 0
  }

//  println(minCostStairClimbing(6, Array(10,15,20,1,13,11)))
  def minCostStairClimbing(n:Int, costArray:Array[Int]): Int = {
    // Can only make 2 steps

    val minCostArray = Array.ofDim[Int](costArray.length)

    // Build base cases
    minCostArray(0) = costArray(0)
    minCostArray(1) = costArray(1)

    for (i <- 2 until costArray.length) {
      minCostArray(i) = costArray(i) + Math.min(minCostArray(i-1), minCostArray(i-2))
    }

    minCostArray(n-1)
  }

//  println(maximumPathSum(Array(Array(1,3,2), Array(2,1,3), Array(10,1,2))))
  def maximumPathSum(grid:Array[Array[Int]]): Int = {
    // Space can be maximized if we reuse grid

    // Empty check
    if (grid.isEmpty) return 0

    // Sizes
    val n = grid.length
    val m = grid(0).length

    // Define output matrix
    val maxGrid = Array.ofDim[Int](n, m)

    // Starting position
    maxGrid(0)(0) = grid(0)(0)

    // Default row
    for (i <- 1 until n) {
      maxGrid(i)(0) = grid(i)(0) + maxGrid(i-1)(0)
    }

    // Default column
    for (j <- 1 until m) {
      maxGrid(0)(j) = grid(0)(j) + maxGrid(0)(j-1)
    }

    // Maximize grid by choosing max
    for (i <- 1 until n) {
      for (j <- 1 until m) {
        maxGrid(i)(j) = grid(i)(j) + Math.max(maxGrid(i - 1)(j), maxGrid(i)(j - 1))
      }
    }

    // Return last value
    maxGrid(n-1)(m-1)
  }

//  println(uniquePathInBoxOptimized(9,5))
  def uniquePathInBoxOptimized(n:Int, m:Int): Int = {
    if (n < 0 || m < 0) return 0

    val table = Array.ofDim[Int](n)

    // Fill with O(n) space
    for (i <- 0 until n) table(i) = 1

    for (_ <- 1 until m) {
      for (i <- 1 until n) {
        table(i) = table(i) + table(i - 1)
      }
    }

    if (table.isEmpty) 0 else table(table.length-1)
  }

//  println(uniquePathInBox(9,5))
  def uniquePathInBox(n:Int, m:Int): Int = {
    if (n < 0 || m < 0) return 0

    val nn = n + 1
    val mm = m + 1
    val table = Array.ofDim[Int](nn, mm)

    for (i <- 1 until nn) table(i)(1) = 1
    for (i <- 1 until mm) table(1)(i) = 1
    for (i <- 2 until nn)
      for (j <- 2 until mm) {
        table(i)(j) = table(i-1)(j) + table(i)(j-1)
      }

    table(n)(m)
  }

//  println(fibTabulationReduction(6))
  def fibTabulationReduction(n:Int):Int = {
    val table = Array.ofDim[Int](3)
    table(0) = 0
    table(1) = 1

    for (i <- 2 to n) {
      table(i % 3) = table((i-1) % 3) + table((i-2) % 3)
    }

    table(n%3)
  }

//  println(fibTabulation(6))
  def fibTabulation(n:Int):Int = {
    val table = Array.ofDim[Int](n+1)
    table(0) = 0
    table(1) = 1

    for (i <- 2 to n) {
      table(i) = table(i-1) + table(i-2)
    }

    table(n)
  }

//  println(fibMemoization(6))
  def fibMemoization(n:Int): Int = {
    import collection.mutable.{Map}

    val memo = Map[Int, Int](0 -> 0, 1 -> 1)

    def permute(x:Int):Int = {
      if (memo.get(x).isDefined) {
        memo(x)
      } else {
        val r = fibMemoization(x-1) + fibMemoization(x-2)
        memo.put(x, r)
        r
      }
    }

    permute(n)
  }

}
