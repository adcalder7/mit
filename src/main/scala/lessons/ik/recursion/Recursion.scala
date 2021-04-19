package lessons.ik.recursion

import scala.collection.mutable

object Recursion extends App {
  import Solution2._

  // Managers 100,000 problem
//  println("People to solve manager issue: ", managerProblem(100000))

//  println(factorial(4))

  // n!
//  printPermutations("ANGEL")

  // choices^size
  // combinatorics
//  printPermutationsWithRepeats("ABC")

//  printPermutationsWithBuffer("AB")

//  println(power(10,10))

//  printAllSubsets("ABC")

//  printSubsets("ABC", 1)

//  println(Solution.fibBad(8))
//
//  println(Solution.fibOkay(8))
//
//  println(Solution.fibBest(8))

//  println(Solution.isPalindrome("a"))

//  countBinaryNumbers(3)
//  println()
//  println(countBinaryNumbersBad(3).mkString("\n"))

//  downUp("hello")

//  printNumbers(3)

//  printPermutationsDFS("ABC")

//  printPermutationsBFS("ABC")

//  subSets("ABC", 2)

//  allSubSets("ABC")

//  subSetWays(Array(1,1,1))

//  println(subsetsWithDupTwoWays(Array(1,1,1)))
}

object Solution2 {

  def subsetsWithDupTwoWays(a: Array[Int]): List[List[Int]] = {
    import collection.mutable.{Buffer, Set}

    val output = Buffer[List[Int]]()
    val buffer = Buffer[Int]()

    // Have to sort first
    val nums = a.sorted

    def permute2(i:Int = 0): Unit = {
      output.append(buffer.toList)
      for (j <- i until nums.length) {
        if (!(j > i && nums(j-1) == nums(j))) {
          buffer.append(nums(j))
          permute2(j+1)
          buffer.remove(buffer.size-1)
        }
      }
    }

    def permute(i:Int = 0): Unit = {
      if (i >= nums.length){
        output.append(buffer.toList)
      } else {
        buffer.append(nums(i))
        permute(i+1)
        buffer.remove(buffer.size-1)

        if (buffer.isEmpty || buffer.last != nums(i)) {
          permute(i+1)
        } else {
          println(nums(i) + " | " + buffer.mkString(","))
        }
      }
    }

    permute()

    output.toList
  }

  // Two ways to generate subsets
  def subSetWays(nums:Array[Int]):Unit = {
    import collection.mutable.Buffer

    // f(n) = f(n, k-1) + f(n-1, k)
    val output = Buffer[List[Int]]()
    val buffer = Buffer[Int]()

    def permute1(i:Int = 0): Unit = {
      output.append(buffer.toList)
      for (j <- i until nums.length) {
        buffer.append(nums(j))
        permute1(j+1)
        buffer.remove(buffer.size-1)
      }
    }

    def permute2(i:Int = 0): Unit = {
      if (i >= nums.size) {
        output.append(buffer.toList)
      } else {
        buffer.append(nums(i))
        permute2(i+1)
        buffer.remove(buffer.size-1)
        permute2(i+1)
      }
    }

    permute1()
    output.foreach(e => print(e.mkString(",") + "|"))
    println()
    buffer.clear()
    output.clear()
    permute2()
    output.foreach(e => print(e.mkString(",") + "|"))
    println()

    output.toList
  }

  def allSubSets(word:String):Unit = {
    val buffer = mutable.Buffer[Char]()

    def permute(n:Int = 0):Unit = {
      if (n >= word.length) {
        println(buffer.mkString(""))
      } else {
        // Choose
        buffer.append(word(n))
        permute(n+1)

        // Not choose
        buffer.remove(buffer.length-1)
        permute(n+1)
      }
    }

    permute()
  }

  def subSets(word:String, size:Int): Unit = {
    val buffer = mutable.Buffer[Char]()

    def permute(start:Int = 0):Unit = {
      if (buffer.length >= size) {
        println(buffer.mkString(""))
      } else {
        for (i <- start until word.length) {
          buffer.append(word(i))
          permute(i+1)
          buffer.remove(buffer.length-1)
        }
      }
    }

    permute()
  }

  // Do not use
  def printPermutationsBFS(word:String):Unit = {
    val buffer = mutable.Buffer[Char]()
    val choices:mutable.Buffer[Char] = word.toBuffer

    def permute(): Unit = {
      if (choices.isEmpty) {
        println(buffer.mkString(""))
      } else {
        for (i <- 0 until choices.length) {
          val choice = choices.remove(i)
          buffer.append(choice)
          permute()
          buffer.remove(buffer.length-1)
          choices.insert(i, choice)
        }
      }
    }

    permute()
  }

  def printPermutationsDFS(word:String):Unit = {
    val buffer:mutable.Buffer[Char] = word.toBuffer

    // DFS
    def permute(pos:Int = 0): Unit = {
      if (pos >= buffer.length) {
        println(buffer.mkString(""))
      } else {
        for (i <- pos until buffer.length) {
          swap(pos, i)
          permute(pos+1)
          swap(pos, i)
        }
      }
    }

    def swap(i:Int, j:Int): Unit = {
      val temp = buffer(i)
      buffer(i) = buffer(j)
      buffer(j) = temp
    }

    permute()
  }

  def printNumbers(n:Int):Unit = {

    def permute(buffer:mutable.Buffer[Int] = mutable.Buffer[Int]()): Unit = {
      if (buffer.length == n) {
        println(buffer.mkString(""))
      } else {
        for (i <- 0 to 9) {
          buffer.append(i)
          permute(buffer)
          buffer.remove(buffer.length-1)
        }
      }
    }

    permute()
  }

  def downUp(word:String): Unit = {

    def permute(cb: mutable.Buffer[Char]): Unit = {
      if (cb.length == 1) {
        println(cb.mkString(""))
      } else {
        println(cb.mkString(""))
        val char = cb.remove(cb.length-1)
        permute(cb)
        cb.append(char)
        println(cb.mkString(""))
      }
    }

    permute(word.toBuffer)
  }

  def countBinaryNumbersBad(n:Int):mutable.Buffer[String] = {

    // BFS
    def permute(n:Int):mutable.Buffer[String] = {
      if (n == 1) {
        mutable.Buffer[String]("0", "1")
      } else if (n > 1) {
        val prev = permute(n-1)
        val result = mutable.Buffer[String]()
        prev.foreach(e => {
          result.append(e + '0')
          result.append(e + '1')
        })
        result
      } else {
        null
      }
    }

    permute(n)
  }

  def countBinaryNumbers(n:Int = 2): Unit = {

    // DFS
    def permute(buffer:mutable.Buffer[Int] = mutable.Buffer[Int]()) {
      if (buffer.length == n) {
        println(buffer.mkString(""))
      } else {
        for (i <- 0 to 1) {
          buffer.append(i)
          permute(buffer)
          buffer.remove(buffer.length-1)
        }
      }
    }

    permute()
  }

  def isPalindrome(word:String): Boolean = {

    def permute(s:mutable.Buffer[Char]):Boolean = {
      if (s.length > 0) {
        val a = s.remove(0)
        val b = s.remove(s.length - 1)
        if (a != b) false
        else permute(s)
      } else true
    }

    permute(word.toBuffer)
  }

  def fibBest(n:Int): BigInt = {

    def fibTailRec(c: Int, i:BigInt, j:BigInt):BigInt = {
      if (c >= n) i
      else fibTailRec(c + 1, i+j, i)
    }

    fibTailRec(2,1,1)
  }

  def fibOkay(n:Int): Long = {

    // * This is dynamic programming because we are going to the bottom
    // of the tree, getting base cases, then doing linear time
    // * Memoization would be to save all data as you go down into an array
    // to then traverse the array linearly with fib equation
    var b:Long = -1
    def dynamicProgramming(i:Int): Long = {
      if (i <= 0) 0 else if (i == 1) 1
      else {
        val a = dynamicProgramming(i-1)
        val c = a + (if (b == -1) {b = dynamicProgramming(i-2); b} else b)
        b = a
        c
      }
    }

    dynamicProgramming(n)
  }

  def fibBad(n:Int): Long = {
    if (n < 0) 0
    else if (n == 1) n
    else {
      fibBad(n-1) + fibBad(n-2)
    }
  }

  def printSubsets(word:String, size: Int): Unit = {
    import scala.collection.mutable

    def permute(buffer: mutable.Buffer[Char] = mutable.ListBuffer.empty, i:Int = 0): Unit = {
      if (buffer.size == size) {
        println(buffer.mkString(","))
      } else if (i >= word.size) {
//        println(buffer.mkString(""))
      } else {
        // Choose
        buffer.append(word(i))
        permute(buffer, i+1)

        // Not choose
        buffer.remove(buffer.length-1)
        permute(buffer, i+1)
      }
    }

    permute()
  }

  // O^(2^n)
  def printAllSubsets(word:String): Unit = {
    import scala.collection.mutable

    def permute(buffer: mutable.ListBuffer[Char] = mutable.ListBuffer.empty, i:Int = 0): Unit = {
      if (i >= word.size) {
        println(buffer.mkString(""))
      } else {
        buffer.append(word(i))
        permute(buffer, i+1)
        buffer.remove(buffer.length-1)
        permute(buffer, i+1)
      }
    }

    permute()
  }

  def power(x:Int, y:Int): Long = {
    if (y == 0) 1
    else if (y % 2 == 0) {
      val m = power(x, y/2)
      m * m
    } else if (y % 3 == 0) {
      val m = power(x, y/3)
      m * m * m
    } else if (y % 5 == 0) {
      val m = power(x, y/5)
      m * m * m * m * m
    } else x * power(x, y - 1)
  }

  def printPermutationsWithRepeats(word:String): Unit = {
    import scala.collection.mutable

    def permute(buffer:mutable.ListBuffer[Char] = mutable.ListBuffer.empty, j:Int = 0): Unit = {
      if (buffer.size == word.size) {
        println(buffer.mkString(","))
      } else {
        for (i <- 0 to word.size-1) {
          buffer.append(word(i))
          permute(buffer, j+1)
          buffer.remove(buffer.size-1)
        }
      }
    }

    permute()
  }

  def printPermutationsWithBuffer(word:String): Unit = {
    import scala.collection.mutable

    def permute(buffer:mutable.ListBuffer[Char], cc:Int = 0): Unit = {
      if (cc >= word.size) {
        println(buffer.mkString(","))
      } else {
        for (i <- cc until buffer.length) {
          swap(buffer, cc, i)
          permute(buffer, cc+1)
          swap(buffer, cc, i)
        }
      }
    }

    def swap(buffer:mutable.ListBuffer[Char], i:Int, j:Int): Unit = {
      val temp = buffer(i)
      buffer(i) = buffer(j)
      buffer(j) = temp
    }

    permute(mutable.ListBuffer(word:_*))
  }

  def printPermutations(s:String):Unit = {
    import scala.collection.mutable

    val list = mutable.ListBuffer(s:_ *)
    val size = list.size-1

    def permute(cc:Int): Unit = {
      if (cc >= size) {
        println(list.mkString(","))
      } else {
        for (i <- cc to size) {
          swap(cc, i)
          permute(cc+1)
          swap(cc, i)
        }
      }
    }

    def swap(i:Int, j:Int): Unit = {
      val temp = list(i)
      list(i) = list(j)
      list(j) = temp
    }

    permute(0)
  }

  def factorialGood(n:Int, acc:BigInt):BigInt = {
    if (n <= 1) acc
    // If the last expression is the function itself
    // Compiler will turn into a tailrec
    else factorialGood(n-1, n*acc)
  }

  def factorialBad(n:Int): Int = {
    if (n == 1) 1
    else n*factorialBad(n-1)
  }

  def managerProblem(n:Int): Int = {
    if (n <= 100) {
      println("n: ", n)
      n
    } else {
      var col = 0
      val div = 10
      for (_ <- 0 until div) {
        col += managerProblem(n/div)
      }
      col
    }
  }

}
