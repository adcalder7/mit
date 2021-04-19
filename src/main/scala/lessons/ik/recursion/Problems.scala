package lessons.ik.recursion

object Problems extends App {

  println(wordBreakCount(
    Array("kick", "start", "kickstart", "is", "awe", "some", "awesome"),
    "kickstartisawesome"))
  def wordBreakCount(dictionary:Array[String], txt:String): Int = {
    import collection.mutable.Buffer

    val d = dictionary.toSet

    var count = 0
    def permute(i:Int = 0, s:String = ""): Unit = {
      if (i < txt.length) {
        val stage = Buffer[Char]()
        for (j <- i until txt.length) {
          stage.append(txt(j))
          val ss = stage.mkString
          if (d.contains(ss)) {
            permute(j+1, s + ss + "-")
          }
        }
      } else {
        count += 1
        println(s)
      }
    }

    permute()
    count % 1000000007
  }

//  println(binarySearch(Array(1,2,3,4,5,6,7,8,9), 8))
  def binarySearch(arr:Array[Int], target:Int):Boolean = {
    val sorted = arr.sorted

    def permute(f:Int = 0, t:Int = sorted.length):Boolean = {
      if (f >= arr.length || t < 0) return false

      val mid = (f+t)/2

      if (sorted(mid) < target) {
        // Go right
        permute(mid+1, t)
      } else if (sorted(mid) > target) {
        // Go left
        permute(f, mid-1)
      } else sorted(mid) == target

    }

    permute()
  }

//  println(check_if_sum_possible(Array(2,-10,10), 0))
  def check_if_sum_possible(arr: Array[Long], k: Long): Boolean = {

    def permute(i:Int = 0, agg:Option[Long] = None): Boolean = {
      if (agg.isDefined && agg.get == k) true
      else {
        var j = i
        var found = false
        while (!found && j < arr.length) {
          val sum = agg.getOrElse(0l) + arr(j)
          found = permute(j+1, Some(sum))
          j += 1
        }
        found
      }
    }

    permute()
  }

//  println(find_all_well_formed_brackets(4).mkString("\n"))
  def find_all_well_formed_brackets(n: Int): Array[String] = {
    val buffer = collection.mutable.Buffer[String]()
    val output = collection.mutable.Buffer[String]()
    val mid = n

    def permute(l:Int, o:Int = 0, c:Int = 0): Unit = {
      if (l == 0) {
        output.append(buffer.mkString)
      } else {
        if (o < mid) {
          buffer.append("(")
          permute(l-1, o+1, c)
          buffer.remove(buffer.length - 1)
        }

        if (o > c) {
          buffer.append(")")
          permute(l-1, o, c+1)
          buffer.remove(buffer.length - 1)
        }
      }
    }

    permute(2*n)

    output.toArray
  }

  //  println(how_many_BSTs(4))
  def how_many_BSTs(n: Int): Long = {
    if (n == 0 || n == 1) 1
    else {
      var sum = 0l
      for (i <- 1 to n) {
        sum += how_many_BSTs(i-1)*how_many_BSTs(n-i)
      }
      sum
    }
  }

//  find_all_arrangements(5).foreach(e => {e.foreach(x => println(x.mkString))})
  def find_all_arrangements(n: Int): Array[Array[String]] = {
    val choices = collection.mutable.Set[(Int, Int)]()
    val board = Array.fill(n, n)("-")
    val output = collection.mutable.Buffer[Array[String]]()

    def permute(queen:Int = 0):Unit = {
      if (queen >= n) {
        output.append(board.map(_.mkString))
        output.append(Array(""))
      } else {
        for (i <- 0 until n) {
          if (canMakeChoice(queen, i)) {
            manageChoice(queen, i, true)
            permute(queen+1)
            manageChoice(queen, i, false)
          }
        }
      }
    }

    def manageChoice(x:Int, y:Int, persist:Boolean):Unit = {
      if (persist) {
        choices.add((x, y))
        draw(x, y, "q")
      } else {
        choices.remove((x, y))
        draw(x, y, "-")
      }
    }

    def draw(x:Int, y:Int, char:String):Unit = board(x)(y) = char

    def canMakeChoice(x2:Int = 0, y2:Int = 0):Boolean = {
      var valid = true

      val iterator = choices.iterator
      while (valid && iterator.hasNext) {
        val (x1, y1) = iterator.next()
        valid = !(x1 == x2 || y1 == y2 || isSlopeOne(x1, y1, x2, y2))
      }

      if (!valid) {
        print("")
      }

      valid
    }

    def isSlopeOne(x1:Int, y1:Int, x2:Int, y2:Int):Boolean = {
      Math.abs((y1-y2)/(x1-x2).toFloat) == 1
    }

    permute()

    output.toArray
  }

//  println(generate_all_subsets("ab").mkString(","))
  def generate_all_subsets(s: String): Array[String] = {
    val buffer = collection.mutable.Buffer[String]()
    val output = collection.mutable.Buffer[String]()

    def permute(i:Int = 0): Unit = {
      if (i >= s.length) {
        output.append(buffer.mkString)
      } else {
        // Choose character
        buffer.append(s"${s(i)}")
        permute(i + 1)
        buffer.remove(buffer.length - 1)

        // Not choose character
        permute(i + 1)
      }
    }

    if (s == null) {
      output.append("")
    } else {
      permute()
    }

    output.toArray
  }

//  println(generate_palindromic_decompositions("abaaa").mkString("\n"))
  def generate_palindromic_decompositions(word: String): Array[String] = {
    val loop = new scala.util.control.Breaks
    val palindromes = collection.mutable.Buffer[String]()
    val buffer = collection.mutable.Buffer[String]()

    def permute(pointer:Int = 0):Unit = {
      if (pointer >= word.length) {
        buffer.remove(buffer.length - 1)
        palindromes.append(buffer.mkString)
        buffer.append("|")
      } else {
        loop.breakable {
          for (i <- 1 to word.length) {
            val end = pointer + i

            // prevent making invalid choices
            if (end > word.size) loop.break()

            val sample = word.substring(pointer, end)
            if (isPalindrome(sample)) {
              buffer.append(sample)
              buffer.append("|")
              permute(end)
              buffer.remove(buffer.length - 1)
              buffer.remove(buffer.length - 1)
            }
          }
        }
      }
    }

    def isPalindrome(s:String): Boolean = {
      def permute(ss:Int = 0, ee:Int = s.length-1): Boolean = {
        if (ss <= ee) {
          if (s(ss) == s(ee)) permute(ss+1, ee-1)
          else false
        } else true
      }
      permute(0, s.length-1)
    }

    permute()

    palindromes.toArray
  }

//  println(generate_all_expressions("222", 24).mkString("\n"))
  def generate_all_expressions(s: String, target: Long): Array[String] = {
    val buffer = collection.mutable.Buffer[Char]()
    val results = collection.mutable.Buffer[String]()

    def permute(p:Int = 0):Unit = {
      if (p >= s.length) {
        val output = buffer.mkString("")
        if (eval(output) == target) {
          results.append(output)
        }
      } else {
        makeChoice(s(p), p+1)
        if (!buffer.isEmpty && buffer.last != '+' && buffer.last != '*') {
          makeChoice('+', p)
          makeChoice('*', p)
        }
      }
    }

    def makeChoice(c:Char, p:Int = 0):Unit = {
      buffer.append(c)
      permute(p)
      buffer.remove(buffer.length-1)
    }

    def eval(s:String):Long =
      s.split("\\+")
        .map(_.split("\\*")
          .map(_.toLong)
          .product)
        .sum

    permute()

    results.toArray
  }

}
