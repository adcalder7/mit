package lessons.leetcode.adhoc

import collection.mutable.{Buffer, ListBuffer, Map}
import scala.collection.mutable

// We don’t provide test cases in this language yet. Please write your code below, and don’t forget to test edge cases!



object Question extends App {

  case class Node(data:Int) {
    var next:Node = null
  }

  def driver():Unit = {
    var root = Node(1)
    root.next = Node(2)
    root.next.next = Node(8)
    root.next.next.next = Node(9)
    root.next.next.next.next = Node(12)
    root.next.next.next.next.next = Node(16)

    getOriginalOrder(root)

    while (root != null) {
      print(root.data + ", ")
      root = root.next
    }

  }
  driver()

  val heap = collection.mutable.PriorityQueue.empty[Int](Ordering.by((i:Int) => -i))

  def getOriginalOrder(node:Node):Unit = {

    def reverseInPlace(start:Node, count:Int, prev:Node):Unit = {
      var p1:Node = prev
      var p2:Node = start
      var c = 0
      while (c < count) {
        val temp = p2.next
        p2.next = p1
        p1 = p2
        p2 = temp
        c += 1
      }
      prev.next = p1
      start.next = p2
    }

    var next:Node = node
    var prev:Node = null
    var start:Node = null
    var count = 0

    while (next != null) {
      if (start == null) {
        if (next.data % 2 == 0) {
          start = next
          count = 0
        }
      } else if (next.data % 2 != 0) {
        reverseInPlace(start, count, prev)
        start = null
        count = 1
      }

      if (start == null) prev = next
      next = next.next
      count += 1
    }

    if (start != null) reverseInPlace(start, count, prev)
  }

}

object Solution extends App {

  // We don’t provide test cases in this language yet, but have outlined the signature for you. Please write your code below, and don’t forget to test edge cases!
  println(rotationalCipher("Zebra-493?", 3))
  def rotationalCipher(input: String, rotationFactor: Int):String = {
    def getNextCharacter(c:Char, r:Int = rotationFactor):Char = {
      if (c.isDigit) {
        // Numbers
        ((c.asDigit + r) % 10).toString()(0)
      } else if (c.isUpper) {
        // Upper
        (((c-'A'+r) % ('Z'-'A'+1)) + 'A').toChar
      } else if (c.isLower) {
        // Lower
        (((c-'a'+r) % ('z'-'a'+1)) + 'a').toChar
      } else c
    }

    val buf = ListBuffer[Char]()
    for (i <- 0 until input.size) {
      buf.append(getNextCharacter(input.charAt(i)))
    }

    buf.mkString("")
  }

  // Rolling hash
  println(strStr("a", "a"))
  def strStr(h: String, n: String): Int = {
    if (h.isEmpty) return if (n.isEmpty) 0 else -1
    else if (n.isEmpty) return 0

    // c*2^n.size + c*2^n.size-1 + ... + c*2^1
    // Rolling
    // ((c*2^n.size + c*2^n.size-1 + ... + c*2^1) - c*2^n.size) * 2
    // Make sure powerIndex gets to 1 not 0

    var powerIndex = n.size
    val overflowCheck = Math.pow(2, 31)

    val nHash = n.foldLeft[Double](0d)((i:Double, j:Char) => {
      val r:Double = i + (j * Math.pow(2, powerIndex))
      powerIndex -= 1
      r % overflowCheck
    })

    var curHash = 0d
    var index = -1
    var i = 0
    powerIndex = n.size
    while (i < h.size && index == -1) {
      curHash = curHash + ((h.charAt(i) * Math.pow(2, powerIndex)) % overflowCheck)
      powerIndex = if (powerIndex > 1) powerIndex - 1 else 1
      i += 1

      val w = i - n.size
      if (w >= 0) {
        if (curHash == nHash) {
          index = w
        }
        // c*26^3 + c*26^2 + c*26^1 - c*26^3 * 26 to bring things up
        curHash = curHash - ((h.charAt(w) * Math.pow(2, n.size)) % overflowCheck)
        curHash = curHash * 2
      }
    }

    index
  }

//  println(calculate_power(10000000,10000000))
  def calculate_power(a: Long, b: Long): Long =  {

    def permute(i:Long = b):Long = {
      if (i == 0) {
        1l
      } else if (i % 2 == 0) {
        val x = permute(i/2)
        x * x % 1000000007
      } else {
        a * permute(i - 1l) % 1000000007
      }
    }

    permute()
  }

//  println(maxSlidingWindow(Array(1,3,1,2,0,5), 3).mkString(","))
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    import collection.mutable.Buffer

    val output = Buffer[Int]()
    val deque = Buffer[Int]()

    for (i <- 0 until nums.size) {
      while (!deque.isEmpty && nums(deque.last) <= nums(i)) deque.dropRightInPlace(1)
      deque.append(i)

      if (i >= k - 1) {
        // out of the window
        if (deque(0) <= i - k) deque.dropInPlace(1)

        output.append(nums(deque(0)))
      }
    }

    output.toArray
  }

//  println(find_max_length_of_matching_parentheses("(((()"))
  def find_max_length_of_matching_parentheses(brackets: String): Int = {
    def scan(f: Int = 0, t: Int = brackets.size - 1, b: Int = 1): (Boolean, Int) = {
      var max = 0
      var open, close = 0
      for (i <- f to t by b) {
        brackets.charAt(i) match {
          case '(' => open += 1
          case ')' => close += 1
        }

        if (open == close) {
          max = Math.max(max, open * 2)
        } else {
          if (b > 0) {
            if (open < close) {
              open = 0
              close = 0
            }
          } else {
            if (open > close) {
              open = 0
              close = 0
            }
          }
        }
      }

      (open == close, max)
    }

    val (same, max) = scan()
    if (same) max else scan(brackets.size - 1, 0, -1)._2
  }

//  println(convertToBaseX(1000000,64))
  def convertToBaseX(n:Int, base:Int):String = {
    import collection.mutable.Buffer

    val digits = Buffer[Char]()
    for {i <- '0' to '9'} digits.append(i)
    for {i <- 'A' to 'Z'} digits.append(i)
    for {i <- 'a' to 'z'} digits.append(i)
    digits.append('-')
    digits.append('_')

    var cur = n
    val buf = Buffer[Char]()
    while (cur >= base) {
      buf.prepend(digits(cur % base))
      cur = cur / base
    }
    buf.prepend(digits(cur))

    buf.mkString
  }

  // When they say contiguous
  // They mean the numbers have to be next to each other so
  // NO SORTING
  /*
   https://leetcode.com/problems/minimum-size-subarray-sum/
   */
  def minSubArrayLen(s: Int, nums: Array[Int]): Int = {

    var left = 0
    var sum = 0
    var min = Integer.MAX_VALUE

    for (right <- 0 until nums.size) {
      // Slide window
      sum += nums(right)

      // When we get above our solution... decrement until we get the minimum
      while (sum >= s) {
        min = Math.min(min, right - left + 1)
        sum -= nums(left)
        left += 1
      }
    }

    if (min != Integer.MAX_VALUE) min else 0
  }

//  println(halvesAreAlike("book"))
  def halvesAreAlike(s: String): Boolean = {
    // Have the same number of vowels

    if (s.size == 0) return false
    else if (s.size % 2 != 0) return false

    val half = s.size / 2
    var c1 = 0
    var c2 = 0

    for (i <- 0 until s.size) {
      s.charAt(i) match {
        case 'a'|'e'|'i'|'o'|'u'|'A'|'E'|'I'|'O'|'U' => {
          if (i < half) c1 += 1
          else c2 += 1
        }
        case _ => ""
      }
    }

    c1 == c2
  }

  // Find first missing positive in an array
  def firstMissingPositive(nums: Array[Int]): Int = {
    if (nums.length == 0) return 1

    /*
        The idea is that we can use the same array
        to mark those numbers that are not present.
        If we replace all outbounds items with 1,
        then we can turn those that are inbound to negative,
        The first non-negative we see is the first smallest positive.

        This is because arrays have indices that incrase.
    */

    var oneFound = false

    // All outbounds turns into 1
    var i = 0
    while (i < nums.size) {
      if (nums(i) == 1 && !oneFound) oneFound = true
      if (nums(i) <= 0 || nums(i) > nums.size) nums(i) = 1
      i+=1
    }

    // If One is missing then return one because its the smallest one
    if (!oneFound) return 1

    // All inbounds turns negative
    i = 0
    while (i < nums.size) {
      val item = Math.abs(nums(i)) - 1
      if (item > -1 && nums(item) > 0) nums(item) *= -1
      i += 1
    }

    // Find the non-negative
    i = 0
    while (i < nums.size && nums(i) < 0) i += 1

    // Add 1 because arrays are 0 based and 0 is not a positive integer
    i + 1
  }

  def topKFrequent(words: Array[String], k: Int): List[String] = {
    case class Word(s: String, f: Int) extends Ordered[Word] {
      override def compare(that: Word): Int = {
        if (this.f == that.f) that.s.compare(this.s)
        else this.f.compare(that.f)
      }
    }

    import collection.mutable.{PriorityQueue, Map}

    val map = Map[String, Int]()
    words.foreach(e => map.put(e, map.get(e).getOrElse(0) + 1))

    val order = Ordering.by((i:Word) => i)
    val maxHeap = PriorityQueue.empty[Word](order)

    map.foreach(e => maxHeap.enqueue(Word(e._1, e._2)))

    (for (_ <- 1 to k) yield maxHeap.dequeue.s).toList
  }

  def arraysIntersection(arr1: Array[Int], arr2: Array[Int], arr3: Array[Int]): List[Int] = {
    import collection.mutable.Buffer

    val output = Buffer[Int]()

    var p1 = 0
    var p2 = 0
    var p3 = 0
    while (p1 < arr1.length && p2 < arr2.length && p3 < arr3.length) {
      if (arr1(p1) == arr2(p2) && arr1(p1) == arr3(p3)) {
        output.append(arr1(p1))
        p2 += 1
        p3 += 1
      } else if (arr1(p1) > arr2(p2)) {
        p2 += 1
      } else if (arr1(p1) > arr3(p3)) {
        p3 += 1
      } else {
        p1 += 1
      }
    }

    output.toList
  }

//  println(minTimeToVisitAllPoints(Array(Array(1,1), Array(3,4), Array(-1,0))))
  def minTimeToVisitAllPoints(points: Array[Array[Int]]): Int = {
    var seconds = 0

    case class Point(var x:Int, var y:Int) {
      def equals(p:Point): Boolean = {
        x == p.x && y == p.y
      }
    }

    def move(p1:Point, p2:Point):Boolean = {
      p1.x += (if (p1.x > p2.x) -1 else if (p1.x != p2.x) 1 else 0)
      p1.y += (if (p1.y > p2.y) -1 else if (p1.y != p2.y) 1 else 0)
      p1.equals(p2)
    }

    for (i <- 1 until points.length) {
      val p1 = Point(points(i-1)(0), points(i-1)(1))
      val p2 = Point(points(i)(0), points(i)(1))
      while (!move(p1, p2)) seconds += 1
    }

    seconds + points.length - 1
  }

//  println(toLowerCase("HeLolOO"))
  def toLowerCase(str: String): String = {
    def isUpper(c:Char):Boolean = c >= 'A' && c <= 'Z'

    str.map(c => {
      if (isUpper(c)) {
        ('a' + (c - 'A')).toChar
      } else c
    }).mkString
  }

//  println(sumOddLengthSubarrays(Array(1,4,2,5,3)))
  def sumOddLengthSubarrays(arr: Array[Int]): Int = {
    var sum = 0

    for (i <- 0 until arr.length) {
      val start = i + 1
      val end = arr.length - i
      val total = start * end

      // Divide by two because we want only the odd sub arrays
      var oddAppearancesOfArrIth = total / 2

      // Because its the odd we may need to add 1
      if (total % 2 == 1) {
        oddAppearancesOfArrIth += 1
      }

      sum += arr(i) * oddAppearancesOfArrIth
    }

    sum
  }

  val matrix = Array(Array(1,2,3), Array(4,5,6), Array(7,8,9))
  rotate(matrix)
//  matrix.foreach(e => println(e.mkString(",")))
  def rotate(matrix: Array[Array[Int]]): Unit = {

  }

//  println(removeFirstDigit(113))
  def removeFirstDigit(n: Int):Int = {
    if(n < 10) 0
    else n%10 + (removeFirstDigit(n/10) * 10)
  }

  // println(smallerNumbersThanCurrent(Array(7,3,3,7,3)).mkString(","))
  def smallerNumbersThanCurrent(nums: Array[Int]): Array[Int] = {
    val freq = Array.ofDim[Int](500)
    val outputArray = Array.ofDim[Int](nums.length)

    // We could have solved this in
    // 1. n^2
    // 2. n log n (using max heap)
    // 3. n using bucket sort

    // O(N)
    for (i <- 0 until nums.length) freq(nums(i)) += 1

    // O(500)
    var count = 0
    for (i <- 0 until freq.length) {
      if (freq(i) != 0) {
        val temp = freq(i)
        freq(i) = count
        count += temp
      }
    }

    // O(N)
    for (i <- 0 until nums.length) {
      outputArray(i) = freq(nums(i))
    }

    outputArray
  }

  class Ledger(s:String) {
    case class Item(c:Char, f:Int)
    val map = Map[Char, Int]()
    val bookKeeping = mutable.PriorityQueue.empty[Item](Ordering.by((i:Item) => i.f))

    s.foreach(c => {
      map.put(c, map.get(c).getOrElse(0) + 1)
    })

    map.foreach(kv => {
      bookKeeping.enqueue(Item(kv._1, kv._2))
    })

    def isEmpty():Boolean = bookKeeping.isEmpty

    private def enqueue(c:Char, f:Int):Unit = {
      if (map(c) > 0) {
        bookKeeping.enqueue(Item(c, map(c)))
      }
    }

    def getNextChar(c:Option[Char] = None):Option[Char] = {
      if (bookKeeping.isEmpty) {
        None
      } else if (bookKeeping.size == 1 || !c.isDefined) {
        val nextItem = bookKeeping.dequeue()
        map.put(nextItem.c, nextItem.f-1)
        enqueue(nextItem.c, nextItem.f-1)
        Some(nextItem.c)
      } else if (c.get == bookKeeping.head.c) {
        val top = bookKeeping.dequeue()
        val nextItem = bookKeeping.dequeue()
        bookKeeping.enqueue(top)
        map.put(nextItem.c, nextItem.f-1)
        enqueue(nextItem.c, map(nextItem.c))
        Some(nextItem.c)
      } else {
        val nextItem = bookKeeping.dequeue()
        map.put(nextItem.c, nextItem.f-1)
        enqueue(nextItem.c, map(nextItem.c))
        Some(nextItem.c)
      }
    }

  }

  def getNonConsecutiveString(s:String):String = {
    if (s.length < 2) return s

    val ledger = new Ledger(s)
    val output = Buffer[Char]()

    // Get next character is log n because of max heap
    var next:Option[Char] = None
    while (!ledger.isEmpty) {
      next = ledger.getNextChar(next)
      output.append(next.get)
    }

    // Check if output is actually non consecutive
    if (output(output.length-1) == output(output.length-2)) "Invalid Input"
    else output.mkString
  }

}
