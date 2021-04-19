package lessons.educative

object DP extends App {

  println("here")
  println(64)

  def minPathSum(grid: Array[Array[Int]]): Int = {
    import collection.mutable.Map

    val cache = Map[String, Int]()

    def permute(i:Int = 0, j:Int = 0, s:Int = grid(0)(0)):Int = {
      val key = i + "|" + j + "s" + s
      if (cache.contains(key)) return cache(key)

      if (i >= grid.size-1 && j >= grid(0).size-1) return s

      // Right
      val right = if (j < grid(0).size-1) {
        permute(i, j+1, s + grid(i)(j+1))
      } else Integer.MAX_VALUE

      // Down
      val down = if (i < grid.size-1) {
        permute(i+1, j, s + grid(i+1)(j))
      } else Integer.MAX_VALUE

      cache.put(key, Math.min(right, down))
      cache(key)
    }

    permute()
  }

  // println(kmp("axxaxa", "aaaa"))
  def kmp(word:String, pattern:String):Boolean = {
    var i = 1
    var j = 0
    val kmpTable = Array.ofDim[Int](pattern.size)

    while (i < pattern.size) {
      if (pattern.charAt(i) == pattern.charAt(j)) {
        kmpTable(i) = j + 1
        i += 1
        j += 1
      } else if (j-1 > -1) {
        // Back track till next match
        j = kmpTable(j-1)
      } else {
        i += 1
      }
    }

    println(kmpTable.mkString(","))

    i = 0
    j = 0
    while (i < word.size && j < pattern.size) {
      if (word.charAt(i) == pattern.charAt(j)) {
        i += 1
        j += 1
      } else if (j-1 > -1) {
        j = kmpTable(j-1)
      } else {
        i += 1
      }
    }

    j == pattern.size
  }

//  println(numberOfPaths(Array(Array(1,1), Array(1,1))))
  def numberOfPaths(matrix: Array[Array[Int]]): Int = {
    import collection.mutable.Map
    import collection.immutable.List

    if (matrix.size != 0 && matrix(0).size != 0) {
      if (matrix(0)(0) == 0) return 0
    }

    val cache = Map[Point, Int]()
    case class Point(x:Int, y:Int)

    def permute(p:Point):Int = {
      if (cache.contains(p)) return cache(p)

      val ways = getValidMoves(p)
      if (ways.isEmpty) {
        if (p.x == matrix.size-1 && p.y == matrix(0).size-1) {
          return 1
        }
        return 0
      }

      var count = 0
      for (m <- ways) {
        count += permute(m)
      }

      cache.put(p, count)
      count
    }

    def getValidMoves(p:Point): List[Point] = {
      var moves = List[Point]()

      // Left
      if (p.x + 1 < matrix.size && matrix(p.x+1)(p.y) != 0) {
        moves = moves :+ Point(p.x+1, p.y)
      }

      // Down
      if (p.y + 1 < matrix(0).size && matrix(p.x)(p.y+1) != 0) {
        moves = moves :+ Point(p.x, p.y+1)
      }

      moves
    }

    permute(Point(0,0))
  }

//  println(max_product_from_cut_pieces(4))
  def max_product_from_cut_pieces(n: Int): Long = {
    import collection.mutable.Map

    val cache = Map[Int, Map[Int, Long]]()

    def permute(l:Int = n, j: Int = 1):Long = {
      if (cache.contains(l) && cache(l).contains(j)) return cache(l)(j)

      if (l == 0) return j
      else if (l < 0) return 0l

      var max = 1l
      for (i <- 1 until n) {
        max = Math.max(max, j * permute(l-i, i))
      }

      cache.getOrElseUpdate(l, Map[Int, Long]()).put(j, max)
      max
    }

    permute()
  }

//  println(knightsTour(1, 3))
  def knightsTour(startdigit: Int, phonenumberlength: Int):Long = {
    import collection.mutable.Map

    /*
    1. Phone numbers
     */

    val cache = Map[Int, Map[Int, Long]]()
    def permute(pos:Int = startdigit, l:Int = phonenumberlength):Long = {
      if (cache.contains(pos) && cache(pos).contains(l)) return cache(pos)(l)

      if (l <= 1) return 1l

      var count = 0l
      for (digit <- getValidNumbers(pos)) {
        count += permute(digit, l-1)
      }

      cache.getOrElseUpdate(pos, Map[Int, Long]()).put(l, count)
      count
    }

    def getValidNumbers(pos:Int):List[Int] = {
      pos match {
        case 1 => List(6,8)
        case 2 => List(7,9)
        case 3 => List(4,8)
        case 4 => List(0,3,9)
        case 5 => List()
        case 6 => List(0,1,7)
        case 7 => List(2,6)
        case 8 => List(1,3)
        case 9 => List(2,4)
        case 0 => List(4,6)
      }
    }

    permute()
  }

//  println(interleavingString("abc", "xyz", "axbyzc"))
  def interleavingString(s1:String, s2:String, s3:String):Boolean = {
    import collection.mutable.Map

    /*
    Lesson learned
    1. Cache is based on the changing parameter of the recursive function
    2. Because you're moving both target at once, you don't need to consider no-op
     */

    val cache = Map[String, Boolean]()

    def permute(i:Int = 0, j:Int = 0, k:Int = 0):Boolean = {
      val key = i + "" + j + "" + k
      if (cache.contains(key)) return cache(key)

      if (i >= s1.size && j >= s2.size && k >= s3.size) return true
      else if (k >= s3.size) return false

      var s1c = false
      if (i < s1.size && s1.charAt(i) == s3.charAt(k)) s1c = permute(i+1, j, k+1)

      var s2c = false
      if (j < s2.size && s2.charAt(j) == s3.charAt(k)) s2c = permute(i, j+1, k+1)

      cache.put(key, s1c || s2c)
      cache(key)
    }

    permute()
  }

//  println(minDistance("horse","ros"))
  def minDistance(word1: String, word2: String): Int = {
    import collection.mutable.Map

    // Still sub-sequence
    // 4 cases
    // insert    => f(i,j) = 1 + f(i,j+1)
    // delete    => f(i,j) = 1 + f(i+1,j)
    // replace   => f(i,j) = 1 + f(i+1,j+1)
    // no-op     => f(i,j) = return f(i+1, j+1)
    // base      => return remainders because those are inserts
    // min(insert, delete, replace)

    val cache = Map[Int, Map[Int, Int]]()

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (cache.contains(i) && cache(i).contains(j)) return cache(i)(j)

      // base
      if (i >= word1.size) return word2.size - j
      else if (j >= word2.size) return word1.size - i

      // no-op
      if (word1.charAt(i) == word2.charAt(j)) {
        return permute(i+1, j+1)
      }

      // insert
      val op1 = 1 + permute(i,j+1)
      // replace
      val op2 = 1 + permute(i+1,j+1)
      // delete
      val op3 = 1 + permute(i+1,j)

      cache.getOrElseUpdate(i, Map[Int, Int]())
        .put(j, Math.min(op3, Math.min(op1, op2)))
      cache(i)(j)
    }

    permute()
  }

//  println(longestAlternatingSubsequence(Array(3,2,1,4)))
  def longestAlternatingSubsequence(a:Array[Int]):Int = {
    import collection.mutable.Map

    /*
    f(i,j,k) = max(1 + f(i+1,i,k'), f(i+1,-1,k''))
    cache = i,j,k
     */

    val cache = Map[String, Int]()
    def permute(i:Int = 0, j:Int = -1, k:Int = 0):Int = {
      val key = i + "" + j + "" + k
      if (cache.contains(key)) return cache(key)

      if (i >= a.length) return 0

      // if 2 then reset
      val dir = getDirection(i,j,k)

      val c1 = if (dir != 2) {
        1 + permute(i+1, i, dir)
      } else 0

      val c2 = permute(i+1, j, dir)

      cache.put(key, Math.max(c1, c2))
      cache(key)
    }

    def getDirection(i:Int, j:Int, k:Int):Int = {
      if (j == -1) return 0
      val isAsc = a(i) >= a(j)
      if (k == 0) {
        if (isAsc) -1 else 1
      } else if (k == -1 && !isAsc) 1
      else if (k == 1 && isAsc) -1
      else 2
    }

    permute()
  }

//  println(longestBitonicSubSequence(Array(4,2,5,9,7,6,10,3,1)))
  def longestBitonicSubSequence(a:Array[Int]):Int = {

    /*
    Bitonic means going up and going down

    Think about it... at every index... look left and look right
    get the max of that...
    Basically treat every i as the peak of the mountain

    Longest decreasing sub array LDS
    Think of the math. If you sum LDS from start to finish and from finish to start
    you'll get the answer

    IMPORTANT: To cache this, you'll need 2 maps for both recursive functions
     */

    // Longest decreasing sub-sequence
    def permute(i:Int = 0, j:Int = -1):Int = {
      if (i >= a.size) return 0

      val c1 = if (j == -1 || a(i) < a(j)) {
        1 + permute(i+1, i)
      } else 0

      val c2 = permute(i+1, j)

      Math.max(c1, c2)
    }

    def permuteRev(i:Int = a.size-1, j:Int = -1):Int = {
      if (i < 0) return 0

      val c1 = if (j == -1 || a(i) < a(j)) {
        1 + permuteRev(i-1, i)
      } else 0

      val c2 = permuteRev(i-1, j)

      Math.max(c1, c2)
    }

    var max = 0
    for (i <- 0 until a.size) {
      val left = permuteRev(i)
      val right = permute(i)
      max = Math.max(max, left + right - 1)
    }

    max
  }

//  println(minimumDeletions("xxa","xaabb"))
  def minimumDeletions(a:String, b:String):Int = {

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (i >= a.length || j >= b.length) return 0

      if (a.charAt(i) == b.charAt(j)) {
        return 1 + permute(i+1, j+1)
      }

      Math.max(permute(i+1,j), permute(i,j+1))
    }

    val lcs = permute()
    // Deletions + Insertions
    (a.length - lcs) + (b.length - lcs)
  }

//  println(subsequencePatternMatchingCount("baxmx", "ax"))
  def subsequencePatternMatchingCount(s:String, p:String):Int = {
    /*
    f(i,j) = sum(f(i+1,j+1), f(i+1,j))
     */

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (j >= p.size) return 1
      else if (i >= s.size) return 0

      val c1 = if (s.charAt(i) == p.charAt(j)) permute(i+1,j+1)
      else 0

      val c2 = permute(i+1,j)

      c1 + c2
    }

    permute()
  }

//  println(longestRepeatingSubsequence("tomorrow"))
  def longestRepeatingSubsequence(s:String):Int = {
    /*
    1. Same as LCS but this case on the same string
     */

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (i >= s.size || j >= s.size) return 0

      val count = if (i != j && s.charAt(i) == s.charAt(j)) {
        1 + permute(i+1, j+1)
      } else {
        Math.max(permute(i+1, j), permute(i, j+1))
      }

      count
    }

    permute()
  }

//  println(minimumDeletionsToMakeArraySorted(Array(4,2,3,6,10,1,12)))
  def minimumDeletionsToMakeArraySorted(a:Array[Int]):Int = {

    // LCIS
    // f(i, j) = max(f(i+1, i), 1 + f(i+1, j))

    def permute(i:Int = 0, j:Int = -1):Int = {
      if (i >= a.size) return 0

      val c = if (j == -1 || a(i) >= a(j)) {
        1 + permute(i + 1, i)
      } else 0

      val nc = permute(i + 1, j)

      Math.max(c, nc)
    }

    a.size - permute()
  }

//  println(giveShortestCommonSupersequence( "abac", "cab"))
  def giveShortestCommonSupersequence(str1: String, str2: String): String = {
    import collection.mutable.Map

    /*
    f(i,j) = f(i+1,j+1)
    f(i,j) = f(i+1,j)
    f(i,j) = f(i,j+1)
    Remember that you have to return the remaining of the characters at the end
     */

    val cache = Map[Int, Map[Int, String]]()

    def permute(i:Int = 0, j:Int = 0):String = {
      if (cache.contains(i) && cache(i).contains(j)) return cache(i)(j)

      if (i >= str1.size) {
        return str2.slice(j, str2.size)
      }

      if (j >= str2.size) {
        return str1.slice(i, str1.size)
      }

      val ans = if (str1.charAt(i) == str2.charAt(j)) {
        str1.charAt(i) + permute(i+1, j+1)
      } else {
        val one = str1.charAt(i) + permute(i + 1, j)
        val two = str2.charAt(j) + permute(i, j + 1)
        if (one.size > two.size) two else one
      }

      cache.getOrElseUpdate(i, Map[Int, String]()).put(j, ans)
      cache(i)(j)
    }

    permute()
  }

//  println(shortestCommonSupersequence("abac", "cab"))
  def shortestCommonSupersequence(str1: String, str2: String): Int = {
    import collection.mutable.Map

    /*
    f(i,j) = f(i+1,j+1)
    f(i,j) = f(i+1,j)
    f(i,j) = f(i,j+1)
     */

    val cache = Map[Int, Map[Int, Int]]()

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (cache.contains(i) && cache(i).contains(j)) return cache(i)(j)

      if (i >= str1.size) {
        return str2.size - j
      } else if (j >= str2.size) {
        return str1.size - i
      }

      // We must cache the whole thing
      val ans = if (str1.charAt(i) == str2.charAt(j)) {
        1 + permute(i+1, j+1)
      } else {
        val one = 1 + permute(i + 1, j)
        val two = 1 + permute(i, j + 1)
        Math.min(one, two)
      }

      cache.getOrElseUpdate(i, Map[Int, Int]()).put(j, ans)
      cache(i)(j)
    }

    permute()
  }

//  println(longestIncreasingSubSequence(Array(4,2,3,6,10,1,12)))
  def longestIncreasingSubSequence(a:Array[Int]):Int = {

    def permute(i:Int = 0, j:Int = -1):Int = {
      if (i >= a.length) return 0

      val c1 = if (j == -1 || a(i) >= a(j)) 1 + permute(i+1, i) else 0
      val c2 = permute(i+1, j)

      Math.max(c1, c2)
    }

    permute()
  }

//  println(maximumCommonSubsequence("abdca", "cbda"))
  def maximumCommonSubsequence(s1:String, s2:String):Int = {
    import collection.mutable.Map

    /*
    a) f(i,j) = 1 + f(i+1, j+1)
    b) f(i,j) = f(i, j+1, 0)
    c) f(i,j) = f(i+1, j, 0)

    max(b, c)
     */

    val cache = Map[Int, Map[Int, Int]]()

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (cache.contains(i) && cache(i).contains(j)) return cache(i)(j)

      if (i >= s1.size || j >= s2.size) return 0

      if (s1.charAt(i) == s2.charAt(j)) {
        return 1 + permute(i+1, j+1)
      }

      val max = Math.max(permute(i, j+1), permute(i+1, j))
      cache.getOrElseUpdate(i, Map[Int, Int]()).put(j, max)
      max
    }

    permute()
  }

//  println(maximumCommonSubstring("passport","ppsspt"))
  def maximumCommonSubstring(s1:String, s2:String):Int = {
    import collection.mutable.Map

    /*
    1. So you're taking detours.
    Detour a) When you have a matching character go and get that substring count
    Detour b) Check count of i+1,j
    Detour c) Check count of i,j+1
    Finally return max(a,b,c)
    f(i, j, c) = max( f(i+1, j+1, c+1), f(i+1, j, 0), f(i, j+1, 0) )
    cache = f(i,j,c)
     */

    val cache = Map[Int, Map[Int, Int]]()

    def permute(i:Int = 0, j:Int = 0, count:Int = 0): Int = {
      if (cache.contains(i) && cache(i).contains(j)) cache(i)(j)

      if (i >= s1.size || j >= s2.size) return count

      // Detour 1: Go deep to find the current common substring
      var levelCount = count
      if (s1.charAt(i) == s2.charAt(j))
        levelCount = permute(i+1, j+1, levelCount + 1)

      // Detour 2 and 3: Go deep and check left and right
      val result = Math.max(levelCount, Math.max(permute(i+1, j), permute(i, j+1)))
      cache.getOrElseUpdate(i, Map[Int, Int]()).put(j, result)
      cache(i)(j)
    }

    permute()
  }

//  println(listAllPalindromePartitions("aab"))
  def listAllPalindromePartitions(s: String): List[List[String]] = {
    import collection.mutable.{Buffer, Map}

    // n^2
    // Partitioning sub-string
    // f(b,e) = (b-e) == pal ? store
    // f(b,e) = (b-e) != pal ? left, right
    // left  = f(b+1,e)
    // right = f(b,e+1)
    // single characters are palindromes
    // whole string can be a palindrome

    val output = Buffer[List[String]]()
    val buffer = Buffer[String]()
    // val cache = Map[String, Boolean]()

    def permute(b:Int = 0, e:Int = s.size-1):Unit = {
      if (b >= s.length) {
        if (!buffer.isEmpty) output.append(buffer.toList)
        return
      }

      for (i <- b to e) {
        val frag = s.substring(b, i+1)
        val isPal = isPalindrome(b, i)
        if (isPal) {
          buffer.append(frag)
          permute(i+1, e)
          buffer.remove(buffer.length-1)
        }
      }
    }

    def isPalindrome(b:Int, e:Int): Boolean = {
      var x = b
      var y = e
      while (x <= y && s.charAt(x) == s.charAt(y)) {
        x += 1
        y -= 1
      }
      x > y
    }

    permute()

    output.toList
  }

//  println(minimumCutToPalindromePartition("aabcdaa"))
  def minimumCutToPalindromePartition(s:String):Int = {
    import collection.mutable.Map

    /*
    1. f(b,e) = min(f(x', 1 + f(x'', e))
    2. Where x' is the substring before border (a) of a|bc
    3. And x'' is the substring after border (bc) a|bc
     */

    val cache = Map[Int, Map[Int, Int]]()

    def permute(b:Int = 0, e:Int = s.size-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (isPalindrome(b, e)) return 0

      // Can we do better than this level
      var currentCut = e - b
      for (i <- b to e) {
        // Single characters are palindrome
        // a|bc
        if (isPalindrome(b, i)) {
          currentCut = Math.min(currentCut, 1 + permute(i+1, e))
        }
      }

      cache.getOrElseUpdate(b, Map[Int, Int]())
        .put(e, currentCut)
      cache(b)(e)
    }

    def isPalindrome(b:Int, e:Int):Boolean = {
      var x = b
      var y = e
      while (x <= y && s.charAt(x) == s.charAt(y)) {
        x += 1
        y -= 1
      }
      x > y
    }

    permute()
  }

  def minInsertions(phrase:String): Int = {
    import collection.mutable.Map

    val cache = Map[Int, Map[Int, Int]]()
    def permute(b:Int = 0, e:Int = phrase.size-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (b == e) return 1

      val result = if (phrase.charAt(b) == phrase.charAt(e)) {
        2 + permute(b+1, e-1)
      } else {
        Math.max(permute(b+1, e), permute(b, e-1))
      }

      cache.getOrElseUpdate(b, Map[Int, Int]()).put(e, result)
      cache(b)(e)
    }

    phrase.size - permute()
  }

//  println(minimumDeletions("leetcode"))
  def minimumDeletions(phrase:String): Int = {
    import collection.mutable.Map

    val cache = Map[Int, Map[Int, Int]]()
    def permute(b:Int = 0, e:Int = phrase.size-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (b == e) return 1

      val result = if (phrase.charAt(b) == phrase.charAt(e)) {
        2 + permute(b+1, e-1)
      } else {
        Math.max(permute(b+1, e), permute(b, e-1))
      }

      cache.getOrElseUpdate(b, Map[Int, Int]()).put(e, result)
      cache(b)(e)
    }

    phrase.size - permute()
  }

//  println(countSubstrings("aab"))
  def countSubstrings(s: String): Int = {
    import collection.mutable.Map

    // single character is palindrome
    // a[???]a if [???] is palindrome then a[???]a is also a palindrome

    val cache = Map[Int, Map[Int, Int]]()

    def permute(b:Int = 0, e:Int = s.size-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (b == e) return 1

      var count = 0
      if (isPalindrome(s, b, e)) count = 1

      count += permute(b+1, e)
      count += permute(b, e-1)
      count -= permute(b+1, e-1)

      cache.getOrElseUpdate(b, Map[Int, Int]()).put(e, count)
      count
    }

    def isPalindrome(st:String, b:Int, e:Int):Boolean = {
      var x = b
      var y = e
      while (x <= y) {
        if (st.charAt(x) != st.charAt(y))
          return false
        x += 1
        y -= 1
      }
      true
    }

    permute()
  }

//  println(longestPalindromeSubString("abadd"))
  def longestPalindromeSubString(phrase:String):Int = {
    import collection.mutable.Map

    /*
    The biggest question to answer is
    1. Given a string... if first == last how do you know
    if the inside is a palindrome?
    e.g P[????]P
    Answer: If you subtract 2 from the current string and compare that length
    with what comes back from the recursion... if they are equal then it is a palindrome

    Therefore the recursive equation is:
    1. f(b,e) = if (b != e) max(f(b+1,e), f(b,e-1))
    2. f(b,e) = if (b == e) if (f(b+1,e-1) is palindrome) 2 + rec-return else 1
     */

    var longest = ""
    val cache = Map[Int, Map[Int, Int]]()
    def permute(b:Int = 0, e:Int = phrase.length-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (b == e) return 1

      if (phrase.charAt(b) == phrase.charAt(e)) {
        val innerLength = e - b - 1
        if (innerLength == permute(b+1, e-1)) {

          if (e-b >= longest.size) longest = phrase.substring(b, e+1)

          cache.getOrElseUpdate(b, Map[Int, Int]()).put(e, innerLength + 2)
          return cache(b)(e)
        }
      }

      cache.getOrElseUpdate(b, Map[Int, Int]())
        .put(e, Math.max(permute(b+1,e), permute(b,e-1)))
      cache(b)(e)
    }

    permute()

    println("here: " + longest)

    0
  }

//  println(longestPalindromicSubSeq("abdbca"))
  def longestPalindromicSubSeq(phrase:String):Int = {
    import collection.mutable.Map

    // Algorithm goes as follows
    // 1. If size = 1; count = 1 (base cases)
    // 2. If first == last are the same; count += 2
    // 3. If first != last;

    // f(b, e) = if (b != e) max(f(b, e-1), f(b-1, e))
    // f(b, e) = if (b == e) 2 + f(b-1, e-1)
    // cache = f(b,e)

    val cache = Map[Int, Map[Int, Int]]()
    def permute(b:Int = 0, e:Int = phrase.size-1):Int = {
      if (cache.contains(b) && cache(b).contains(e)) return cache(b)(e)

      if (b > e) return 0
      else if (b == e) return 1

      val result = if (phrase.charAt(b) == phrase.charAt(e)) {
        2 + permute(b+1, e-1)
      } else {
        Math.max(permute(b+1, e), permute(b, e-1))
      }

      cache.getOrElseUpdate(b, Map[Int, Int]()).put(e, result)
      cache(b)(e)
    }

    permute()
  }

//  println(paintHouseMinCost(Array(Array(3,5,3),Array(6,17,6),Array(7,13,18),Array(9,10,18))))
  def paintHouseMinCost(costs: Array[Array[Int]]): Int = {
    import collection.mutable.Map

    // f(n,c) = min(a(n)(r) + f(n-1,r), a(n)(b) + f(n-1,b), a(n)(g) + f(n-1,g))
    // Memo = f(n,c)

    val cache = Map[Int, Map[Int, Int]]()

    def permute(n:Int = costs.length-1, c:Int = -1):Int = {
      if (cache.contains(n) && cache(n).contains(c)) return cache(n)(c)

      if (n < 0) return 0

      val cost1 = if (c != 0) costs(n)(0) + permute(n-1, 0) else Integer.MAX_VALUE
      val cost2 = if (c != 1) costs(n)(1) + permute(n-1, 1) else Integer.MAX_VALUE
      val cost3 = if (c != 2) costs(n)(2) + permute(n-1, 2) else Integer.MAX_VALUE

      cache.getOrElseUpdate(n, Map[Int, Int]())
        .put(c, Math.min(Math.min(cost1, cost2), cost3))
      cache(n)(c)
    }

    permute()
  }

//  println(houseThief(Array(2, 10, 14, 8, 1)))
  def houseThief(items:Array[Int]): Int = {
    import collection.mutable.Map

    // f(n) = max(f(n-1), f(n-2) + p(n))

    val cache = Map[Int, Int]()
    def permute(n:Int = items.length-1):Int = {
      if (cache.contains(n)) return cache(n)

      if (n < 0) return 0

      val profit1 = permute(n-1)
      val profit2 = permute(n-2)

      cache.put(n, Math.max(profit1, profit2 + items(n)))
      cache(n)
    }

    permute()
  }

//  println(minJumpsWithFee(Array(1,0,0,0,0), Array(1,2,3)))
  def minJumpsWithFee(fees:Array[Int], steps:Array[Int] = Array(1,2)): Int = {
    import collection.mutable.Map

    // f(n,s) = min(f(n, s'), f(n-1,s))
    // Memo: f(s)
    val cache = Map[Int, Int]()

    def permute(i:Int):Int = {
      if (cache.contains(i)) return cache(i)

      if (i >= fees.length) 0
      else if (i == fees.length-1) fees(i)
      else {
        var levelCost = Integer.MAX_VALUE
        for (j <- 0 until steps.length) {
          levelCost = Math.min(levelCost, permute(i + steps(j)))
        }
        cache.put(i, levelCost + fees(i))
        cache(i)
      }
    }

    var min = Integer.MAX_VALUE
    for (step <- steps) {
      min = Math.min(min, permute(step-1))
    }
    min
  }

//  println(countJumps(Array(3,2,1,1,4)))
  def countJumps(a:Array[Int]):Int = {
    import collection.mutable.Map

    // f(n) = min(f(steps))

    val cache = Map[Int, Int]()
    def permute(j:Int = 0):Int = {
      if (j == a.length-1) return 0
      else if (j >= a.length) return Integer.MAX_VALUE

      if (cache.contains(j)) return cache(j)

      var minLevel = Integer.MAX_VALUE
      for (i <- 1 to a(j)) {
        minLevel = Math.min(minLevel, permute(j + i))
      }

      cache.put(j, if (minLevel == Integer.MAX_VALUE)
        Integer.MAX_VALUE else minLevel + 1)
      cache(j)
    }

    val r = permute()
    if (r == Integer.MAX_VALUE) -1 else r
  }

//  println(numberFactors(Array(1,3,4), 4))
  def numberFactors(factors:Array[Int], n:Int):Int = {
    import collection.mutable.Map

    // f(n) = sum(f(n - factors(i))

    val cache = Map[Int, Int]()

    def permute(nn:Int = n): Int = {
      if (nn == 0) return 1
      else if (nn < 0) return 0

      var sum = 0
      for (i <- 0 until factors.length) {
        sum += permute(nn - factors(i))
      }

      cache.put(nn, sum)
      cache(nn)
    }

    permute()
  }

//  println(countWaysToClimb(Array(1,2,3), 4))
  def countWaysToClimb(steps: Array[Int], n: Int): Long = {
    // Write your code here

    def permute(i:Int = 0, c:Int = 0):Int = {
      if (c == n) return 1
      else if (c > n || i >= steps.size) return 0

      permute(i, steps(i) + c) + permute(i+1, c)
    }

    permute()
  }

//  println(stairCase(Array(1,2,3), 4))
  def stairCase(steps:Array[Int], top:Int):Int = {
    import collection.mutable.Map

    // f(n) = sum(f(n-steps(i))
    val cache = Map[Int, Int]()

    def permute(n:Int = 0):Int = {
      if (cache.contains(n)) return cache(n)

      if (n > top) return 0
      else if (n == top) return 1

      var sum = 0
      for (i <- 0 until steps.length) {
        sum += permute(n + steps(i))
      }

      cache.put(n, sum)
      cache(n)
    }

    permute()
  }

//  println(fibonacci(6))
  def fibonacci(n:Int): Int = {
    import collection.mutable.Map

    // f(n) = sum(f(n-1), f(n-2))
    val cache = Map[Int, Int]()

    def permute(np:Int = n):Int = {
      if (cache.contains(np)) return cache(np)
      if (np == 2) return 1
      else if (np == 1) return 1
      else if (np == 0) return 0

      cache.put(np, permute(np-1) + permute(np-2))
      cache(np)
    }

    permute()
  }

//  println(maxRibbonCut(Array(2,3,5), 5))
  def maxRibbonCut(a:Array[Int], t:Int):Int = {
    import collection.mutable.Map

    // We're trying to get the maximum cuts
    // f(n,t) = max(f(n,t'), f(n-1,t))

    val cache = Map[Int, Int]()

    def permute(n:Int = a.length-1, tt:Int = 0): Int = {
      if (cache.contains(tt)) return cache(tt)

      if (tt == t) return 0
      else if (n < 0 || tt > t) return -1

      var max1 = permute(n, tt + a(n))
      if (max1 != -1) max1 += 1

      val max2 = permute(n-1, tt)

      cache.put(tt, Math.max(max1, max2))
      cache(tt)
    }

    permute()
  }

//  println(minCoinChange2(Array(357,239,73,52), 9832))
  def minCoinChange2(a:Array[Int], t:Int):Int = {
    import collection.mutable.Map

    // f(n,t) = min(f(n,t-1), f(n-1,t))

    val cache = Map[Int, Map[Int, Int]]()

    def permute(i:Int = 0, st:Int = t):Int = {
      if (cache.contains(i) && cache(i).contains(st))
        return cache(i)(st)

      if (st == 0) return 0
      else if (st < 0 || i >= a.length) return Integer.MAX_VALUE

      var min1 = permute(i, st - a(i))
      if (min1 != Integer.MAX_VALUE) min1 += 1

      val min2 = permute(i+1, st)

      cache.getOrElseUpdate(i, Map[Int, Int]()).put(st, Math.min(min1, min2))
      cache(i)(st)
    }

    val result = permute()
    if (result == Integer.MAX_VALUE) -1 else result
  }

//  println(minCoinChange(Array(1,2,5), 10))
  def minCoinChange(a:Array[Int], t:Int): Int = {
    import collection.mutable.Map

    /*
    Lessons learned
    1. Make sure you know what your sub-problem is going to be
    before you code the solution
     */

    if (t < 1) return 0

    // f(n,t) = min(f(n, t'), f(n-1, t))

    val cache = Map[Int, Int]()

    def permute(st: Int = t):Int = {
      if (st < 0) -1
      else if (st == 0) 0
      else {
        if (cache.contains(st)) return cache(st)

        var min = Integer.MAX_VALUE
        for (i <- 0 until a.length) {
          val change = permute(st - a(i))
          if (change >= 0 && change < min)
            min = change + 1
        }

        val xx = if (min == Integer.MAX_VALUE) -1 else min
        cache.put(st, xx)
        cache(st)
      }
    }

    permute()
  }

//  println(coinChange(Array(3,2,1), 5))
  def coinChange(a:Array[Int], t:Int): Int = {
    import collection.mutable.Buffer

    /*
    Lessons learned
    1. When you're generating numbers without repetitions
    pass index so your options can start where index is.
    This works because recursively those numbers before index
    have been considered already
    2. Cache: j, st
     */

    // f(n,t) = sum(f(n-1,t), f(n-1,t-1))

    val buffer = Buffer[Int]()
    def permute(j:Int = 0, st:Int = t):Int = {
      if (st == 0) {
        println(buffer.mkString)
        1
      } else if (st < 0) 0
      else {
        var levelCount = 0
        for (i <- j until a.length) {
          buffer.append(a(i))
          levelCount += permute(i, st - a(i))
          buffer.remove(buffer.size-1)
        }
        levelCount
      }

    }

    permute()
  }

//  println(rodCutting(Array(1,2,3,4,5), Array(2,6,7,10,13), 8))
  def rodCutting(l:Array[Int], p:Array[Int], s:Int):Int = {
    import collection.mutable.Map
    // f(n,l,p) = max(f(n,l',p'), f(n-1,l,p))
    // cache = l,p
    // The reason why we don't choose n is because
    // we can make the same choice

    val cache = Map[Int, Map[Int, Int]]()

    def permute2(i:Int = 0, ll:Int = s, pp:Int = 0): Int = {
      if (cache.contains(ll) && cache(ll).contains(pp)) {
        return cache(ll)(pp)
      }

      if (ll < 0 || i >= l.length) return -1
      if (ll == 0) return pp

      val p1 = permute2(i, ll - l(i), pp + p(i))
      val p2 = permute2(i+1, ll, pp)

      cache.getOrElseUpdate(ll, Map[Int, Int]()).put(pp, Math.max(p1, p2))
      cache(ll)(pp)
    }

    permute2()
  }

//  println(unboundedKnapsack(Array(1,2,3), Array(15,20,50), 5))
  def unboundedKnapsack(w:Array[Int], p:Array[Int], maxWeight:Int): Int = {
    import collection.mutable.Buffer

    /*
    Lessons learned:
    1. There are two ways of doing this. One with two recursive calls having
    the weight change when choosing or skipping an option by doing i+1
    Two, use a for loop but the base case is sp when weight reaches the end
    2. Cache = f(profit,weight)
     */

    // f(n,k) = max(f(n-1,w), f(n-1,w'))
//    def permute2(sp: Int = 0, sw:Int = 0, i:Int = 0): Int = {
//      if (i >= p.size) return sp
//      else if (sw > maxWeight) return 0
//
//      var levelProfit = sp
//      val profit1 = permute(sp, sw, i+1)
//
//      val nw = sw + w(i)
//      val profit2 = if (nw <= maxWeight) permute(sp + p(i), nw, i) else 0
//
//      Math.max(profit1, profit2)
//    }

    val buffer = Buffer[Int]()

    def permute(sp: Int = 0, sw:Int = 0): Int = {
      var lp = sp
      for (i <- 0 until p.length) {
        val nw = sw + w(i)
        buffer.append(w(i))
        if (nw <= maxWeight) {
          val ex = permute(sp + p(i), nw)
          lp = Math.max(lp, ex)
        }
      }
      lp
    }

    println(buffer.mkString(","))
    permute()
  }

//  println(subSetSignatures(Array(1,2,7,1), 9))
  def subSetSignatures(a:Array[Int], target:Int):Int = {

    // + + + +
    // - + + + etc...

    def permute(i:Int = 0, subSum:Int = 0): Int = {
      if (i < a.length) {
        permute(i+1, subSum + a(i)) + permute(i+1, subSum - a(i))
      } else {
        if (subSum == target) 1
        else 0
      }
    }

    permute()
  }

//  println(countSubsetSum(Array(1,1,2,3), 1))
  def countSubsetSum(a:Array[Int], target:Int): Int = {

    /*
    Lessons learned:
    1. Just remember where to put your i < a.length
    2. Watch out for your target
     */

    // Cache is f(i, subSum)
    def permute(i:Int = 0, subSum:Int = 0):Int = {
      if (subSum == target) 1
      else if (subSum < target && i < a.length) {
        permute(i+1, subSum) + permute(i+1, subSum+a(i))
      } else 0
    }

    permute()
  }

//  println(canPartitionKSubsets(Array(4,3,2,3,1,2,5), 4))
  def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {

    /*
    Lessons learned
    1. IMPORTANT: The sum of the numbers in the array
    divided by k is your target sum!!!
    2. IMPORTANT: Dive in the recursion everytime you
    find one k and call the next recursion with k-1
    3. IMPORTANT: sum % k has to be 0
    4. This is a visited/unvisited type problem
     */
    val sum = nums.sum
    if (sum % k != 0) return false
    val targetSum = sum/k
    val visited = Array.fill[Boolean](nums.length)(false)

    def permute(i: Int = 0, subSum:Int = 0, subK:Int = k):Boolean = {
      if (subK == 0) return true

      // If this tree contains targetSum
      // stop and dive into the next tree
      if (subSum == targetSum) {
        // Dive into a different tree
        return permute(0, 0, subK-1)
      }

      // Dive in the tree once you find your sum
      for (j <- i until nums.length) {
        if (!visited(j)) {
          val nextSum = subSum + nums(j)
          if (nextSum <= targetSum) {
            visited(j) = true
            if (!permute(j+1, nextSum, subK)) {
              visited(j) = false
            } else return true
          }
        }
      }

      false
    }

    permute()
  }

//  println(minimumAbsoluteDifference(Array(1,1,1,1)))
  def minimumAbsoluteDifference(a:Array[Int]): Int = {
    import collection.mutable.Map

    /*
    Lessons learned
    1. Be careful with your recursive equation. This one is a minimum problem
    2. f(n,s1,s2) = min(f(n-1,s1,s2), f(n-1,s1',s2'))
    3. Make sure the entire array is not covered. We're looking for a subset
    This means that at every level you have to keep selecting. Hence why we need cache
    4. Need to plant the problem after getting recursive equation
    5. The recursive equation has parameters!!! pay attention to them
    6. You failed at getting the base case for the recursive equation!
    7. Say: "When I get to the bottom, what am I going to do
     */

    val sum = a.sum

    def permute(i:Int = 0, sum1: Int = sum, sum2: Int = 0): Int = {
      // Whatever sum got to the bottom. Return the difference
      if (i == a.length) return Math.abs(sum1 - sum2)

      // 0/1 will take care of getting all the differences
      val diff1 = permute(i + 1, sum1, sum2)
      val diff2 = permute(i + 1, sum1-a(i), sum2+a(i))

      // We just need to get the minimum one
      Math.min(diff1, diff2)
    }

    permute()
  }

//  println(letterCombinations("").mkString(","))
  def letterCombinations(digits: String): List[String] = {
    import collection.mutable.{Map, Buffer}

    /*
    Lesson learned:
    1. When you're doing permutations the equation is f(n) = c + f(n-1)
    2. There is no cache in displaying all combinations since there is no sub-problems
    3. Choices are not picked twice
    4. The result size is equal to the choice size
     */

    val map = Map[Int, String]()

    for (i <- 2 to 9) {
      i match {
        case 2 => map.put(i,"abc")
        case 3 => map.put(i,"def")
        case 4 => map.put(i,"ghi")
        case 5 => map.put(i,"jkl")
        case 6 => map.put(i,"mno")
        case 7 => map.put(i,"pqrs")
        case 8 => map.put(i,"tuv")
        case 9 => map.put(i,"wxyz")
      }
    }

    val buffer = Buffer[Char]()
    val output = Buffer[String]()

    def permute(i:Int = 0): Unit = {
      if (i != 0 && buffer.length >= digits.length) {
        output.append(buffer.mkString)
      } else if (i < digits.length) {
        map(digits(i).asDigit).foreach(c => {
          buffer.append(c)
          permute(i+1)
          buffer.remove(buffer.length-1)
        })
      }
    }

    permute()

    output.toList
  }

//  combinationSum(Array(1,2), 4).foreach(e => println(e.mkString(",")))a
  def combinationSum(c: Array[Int], target: Int): List[List[Int]] = {
    import collection.mutable.{Map, Buffer}

    /*
    Lessons learned:
    1. Is not a 0/1 problem because choices are the same on each sub-problem
    2. Cache is composed of Map[i, Buffer[List[Int]] because s' is the factor and
    it can collide with ub-problem because the same s' can arrive at different sub-problems
    3. We have to sort so we can quit selecting faster when no more selections make sense
    4. You don't have to iterate through cache to return output
     */

    val output = Buffer[List[Int]]()
    val buffer = Buffer[Int]()
    val cache = Map[Int, Buffer[List[Int]]]()

    val candidates = c.sorted

    def permute(i:Int = 0, s:Int = 0): Unit = {
      if (cache.contains(s)) {
        if (s == target) {
          cache(s).append(buffer.toList)
          output.append(buffer.toList)
        }
        return
      }

      if (s == target) {
        cache
          .getOrElseUpdate(s, Buffer[List[Int]]())
          .append(buffer.toList)
        output.append(buffer.toList)
      } else if (s < target) {
        var j = i
        var sum = s
        while (j < candidates.length && sum < target) {
          sum = s + candidates(j)
          buffer.append(candidates(j))
          permute(j, s + candidates(j))
          buffer.remove(buffer.length - 1)
          j += 1
        }
      }
    }

    permute()

    output.toList
  }

//  println(canPartition(Array(1,1)))
  def canPartition(o: Array[Int]): Boolean = {
    import collection.mutable.Map

    val sum = o.sum

    // We're dividing by 2 partitions therefore our sum has to be even
    if (sum % 2 != 0) return false
    val half = sum / 2
    val cache = Map[Int, Map[Int, Boolean]]()

    // f(n,s) = f(n-1, s) || f(n-1, s') {s' = s - o; s' >= h, k>1}
    // cache  = table(n-1, s or s')
    def permute(i:Int = o.length, s:Int = sum): Boolean = {
      if (cache.contains(i) && cache(i).contains(s)) return cache(i)(s)

      val isFound = {
        if (i < 1) false
        else if (s == half) true
        else if (s < half) false
        else permute(i - 1, s) || permute(i - 1, s - o(i-1))
      }

      cache.getOrElseUpdate(i, Map[Int, Boolean]()).put(s, isFound)
      isFound
    }

    permute()
  }

//  println(combinationSum4(Array(1,2,3), 4))
  def combinationSum4(nums: Array[Int], target: Int): Int = {
    import collection.mutable.Map

    val cache = Map[Int, Int]()
    def permute(i:Int = nums.length-1, cp:Int = target): Int = {
      if (cache.contains(cp)) return cache(cp)

      val levelCount = if (cp == 0) 1
      else if (cp < 0) 0
      else {
        var tempCount = 0
        for (o <- 0 until nums.length) {
          tempCount += permute(i-1, cp - nums(o))
        }
        tempCount
      }

      cache.put(cp, levelCount)
      levelCount
    }

    permute()
  }

  // Given a list of profits, constraints and a capacity, find the maximum profit
//  knapsack(Array(1,6,10,16), Array(1,2,3,5), 7)
  def knapsack(p: Array[Int], w: Array[Int], c: Int):Unit = {
    /*
    Depth: c
    Breath: p

    Because we have the option to choose an item in p or not this becomes nCk
    top-down   => f(n) = max(p(n) + f(n-1, c-w(n)), f(n-1, c))
    bottom-up  => f(i) = max(p(i) + f(i-1, c-w(i)), f(i-1, c))
    T => O(2^n)
    S => O(n)

    cache:
    T => O(c*n)
    S => c*n + n => O(c*n)
     */

    val cache = Array.fill(p.length, c + 1)(-1)
    def topBottom(i:Int = p.length-1, cc:Int = 0): Int = {
      if (i < 0) return 0

      if (cache(i)(cc) != -1) cache(i)(cc)
      var levelProfit = 0

      val profit1 = topBottom(i-1, cc)
      val profit2 = if (cc+w(i) <= c) p(i) + topBottom(i-1, cc+w(i)) else 0
      levelProfit = Math.max(profit1, profit2)

      cache(i)(cc) = levelProfit
      levelProfit
    }

    val table = Array.fill(p.length, c + 1)(-1)
    def bottomUp(): Int = {

      // Best cases
      for (pp <- 0 until p.length) table(pp)(0) = 0

      // Go through you limits
      // And if a limit fits for the first profit add it
      // It does not matter how much capacity you increase, you can always fit the first item
      for (cc <- 1 to c) if (table(0)(cc) <= cc) table(0)(cc) = p(0)

      for (i <- 1 until p.length) {
        for (cc <- 1 to c) {
          val profit1 = table(i-1)(cc)
          /*
          The reason why it's called bottom up is because we're solving
          each problem at the smallest possible problem. Then we cache.
          Notice how profit2 is guarded by the current weight.
          - My current weight LIMIT needs to be higher or equal to my proposed weight
          - If we can fit it in then choose it
           */
          val profit2 = if (cc >= w(i)) p(i) + table(i-1)(cc - w(i)) else 0
          table(i)(cc) = Math.max(profit1, profit2)
        }
      }

      table(table.length-1)(c)
    }

    println("top-down: " + topBottom())
    println("bottom-up: " + bottomUp())
    println("--------------------------")

    val cache1 = Array.fill(p.length, c + 1)(-1)
    def topBottomPrintSelection(i:Int = 0, parentWeight:Int = 0): (Int, Int, String) = {
      if (i >= p.length) return (0, 0, "")

      if (cache1(i)(parentWeight) != -1) cache1(i)(parentWeight)
      var levelProfit = 0
      var chooseSelection = s"$i"

      // Don't select thus keep the same weight
      // But my sub-problem could change the weight so include it
      val (profit1, subProblemWeight2, notChooseSelection) = topBottomPrintSelection(i+1, parentWeight)

      // Select and get new level weight
      var levelWeight = parentWeight + w(i)
      val profit2 = if (levelWeight <= c) {
        val (subProblemProfit, subProblemWeight, tempChooseSelection) = topBottomPrintSelection(i+1, levelWeight)
        if (!tempChooseSelection.isEmpty) chooseSelection = chooseSelection + "-" + tempChooseSelection
        levelWeight = subProblemWeight + w(i)
        p(i) + subProblemProfit
      } else {
        levelWeight = parentWeight
        0
      }

      levelProfit = Math.max(profit1, profit2)

      var finalSelection = ""
      if (profit1 > profit2) {
        levelWeight = subProblemWeight2
        finalSelection = notChooseSelection
      } else {
        levelWeight = levelWeight
        finalSelection = chooseSelection
      }

      cache1(i)(parentWeight) = levelProfit
      (levelProfit, levelWeight, finalSelection)
    }

    println(topBottomPrintSelection())
  }

}
