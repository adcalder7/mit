package lessons.hackerrank

import Util.Util.{ListNode, TreeNode, printLinkedList}

object main extends App {
  import Solution._

  //  var list = Array[Int](1,1,3)
  //  val length = Solution.removeDuplicates(list)
  //  print(s'$length ${list.slice(0, length).mkString(', ')}')

  //  var list = Array[Int](1,2,3,4,5,6,7)
  //  println(Solution.maxProfit(list))

  //  var list = Array[Int](1,2,3,4,5,6)
  //  Solution.rotate(list, 7)
  //  println(list.mkString(', '))

  //  var list = Array[Int](3,1)
  //  println(Solution.containsDuplicate(list))

  //  var list = Array[Int](9,2,2,9,-1,0,-2,-2,0)
  //  println(Solution.singleNumber(list))

  //  println(Solution.intersect(Array[Int](2,2,2,3), Array(1,2,3,2)).mkString(', '))

  //  var list = Array[Int](9)
  //  println(Solution.plusOne(list).mkString(', '))

  //  var list = Array[Int](0,1,0,3,1,1,1,1,12,0)
  //  Solution.moveZeroes(list)
  //  println(list.mkString(', '))

  //  var list = Array[Array[Char]](
  //    Array('.','.','.','.','.','.','5','.','.'),
  //    Array('.','.','.','.','.','.','.','.','.'),
  //    Array('.','.','.','.','.','.','.','.','.'),
  //    Array('9','3','.','.','2','.','4','.','.'),
  //    Array('.','.','7','.','.','.','3','.','.'),
  //    Array('.','.','.','.','.','.','.','.','.'),
  //    Array('.','.','.','3','4','.','.','.','.'),
  //    Array('.','.','.','.','.','3','.','.','.'),
  //    Array('.','.','.','.','.','5','2','.','.'))
  //  println(Solution.isValidSudoku(list))

  //    var list = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
  //    Solution.rotate(list)
  //    println("---------")
  //    list.foreach(a => println(a.mkString(", ")))

  //  val list = Array('a','b','c','d','e')
  //  Solution.reverseString(list)
  //  println(list.mkString(", "))

  //  println(Solution.reverse(1534236469))

  //  println(Solution.firstUniqChar("cc"))

  //  println(Solution.isAnagram("bb", "cb"))

  //  println(Solution.isPalindrome("A man, a plan, a canal: Panama"))

  //  println(Solution.myAtoi("21474836460"))

  //  println(Solution.strStr("mississippi", "issipi"))

  //  println(Solution.countAndSay(2))

//  println(Solution.longestCommonPrefix(Array("", "")))

//  printLinkedList(Solution.removeNthFromEnd(Util.getLinkedList(), 1))

//  Util.printLinkedList(Solution.reverseList(Util.getLinkedList()))

//  Util.printLinkedList(Solution.mergeTwoLists(Util.getLinkedList(5, 20, 1), Util.getLinkedList(1, 10, 1)))

//  println(Solution.isPalindrome(Util.getLinkedList(1,1)))

//  println(Solution.hasCycle(Util.getLinkedList(1,1)))

//  println(Solution.maxDepth(Util.buildTree()))

//  Util.printTreeInOrder(Util.buildTree())

//  println(Solution.isValidBST(Util.buildTree()))

//  println(Solution.isSymmetric(Util.buildTree()))

//  Solution.levelOrder(Util.buildTree())

//  Util.printTreeInOrder(Solution.sortedArrayToBST(Array(1,2,3,4,5,6,7,8,9)))

//  println(Solution.binarySearch(Array(1,2,3,4,5,6,7,8), 4))

//  val nums = Array(9,18,1,6,5,4,3,-2,1)
//  Solution.bubbleSort(nums)
//  println(nums.mkString(", "))

//  Solution.mergeSort(nums);
//  println(nums.mkString(", "))

//  println(Solution.firstBadVersion(13))

//  println(Solution.climbStairs(4))

//  println(Solution.maxProfit2(Array(7,4,1,2)))

//  Solution.insertionSort(Array(9,8,7,6,5,4,3,2,1))

//  Solution.mergeSort(Array(5,6,7,8,9,1,2,3,4))

//  Solution.quickSort(Array(9,8,7,6,5,4,3,2,1))

//  println(Solution.mergeArrays(Array(Array(3), Array(9,8,7))).mkString(", "))

//  println(Solution.groupNumbersDivisibleBy2OnLeft(Array(1,2,3,4)).mkString(", "))

//  println(Solution.mergeSort(Array(4,0,1,3,2)).mkString(", "))

//  println(Solution.findZeroSum(Array(3,0,0,0)).mkString("\n"))

//  println(Solution.merge_sort(Array(9,8,7,6,5,4,3,2,1)).mkString(", "))

//   val queue = scala.collection.mutable.PriorityQueue.empty[Int](Ordering.by((i:Int) => i))
//  queue.enqueue(1)
//  queue.enqueue(2)
//  println(queue.mkString(","))

//  println("1".compareTo("2"))

//  println(Solution.bigSorting(Array("1", "2", "3084193741082938", "111" , "200")).mkString(", "))

//  Solution.insertionSort1(2, Array(1,4,3,5,6,2))

//  println(Solution.dutch_flag_sort(Array('R','G','G','G','R','R','B','R')).mkString(","))

//  Solution.merger_first_into_second(Array(1,2,3), Array(4,5,6,0,0,0))

//  Solution.dd(Array(4,5,3,7,2))

//  Solution.countingSort(Array(1,99,1,1,1,1))

//  val count = Array.fill[List[String]](100)(List[String]())

//  println(Solution.insertionSortAnalysis(Array(2,1,3,1,2)))

//  println(Solution.closestNumbers(Array(-5,15,25,71,63)).mkString(" "))

//  println(Solution.activityNotifications(Array(2,2,2,2,2,2,2,2,2,2), 5))

  val aa = Array(3,4,2,5,1)
//  println(Solution.lilysHomework(aa))
  println(quickSort2(aa))
  println(aa.mkString(","))
}

object Solution {

  def lilysHomework(arr: Array[Int]): Int = {
    import scala.collection._
    if (arr == null || arr.isEmpty) return 0

    // 2 * ((n log n) + n)
    def swapCount(array:Array[Int], order:Short = 1):Int = {
      var swaps = 0

      // n log n
      val copy = array.clone
      val arraySorted = arr.sorted(Ordering.by((i:Int) => i*order))

      val map = mutable.Map[Int, mutable.ListBuffer[Int]]()
      for (i <- 0 until copy.length) {
        map.put(arr(i), map.getOrElse(copy(i), mutable.ListBuffer()) += i)
      }

      for (i <- 0 until copy.length) {
        if (arraySorted(i) != copy(i)) {
          swap(arraySorted(i), copy(i))
          swaps += 1
        }
      }

      def swap(key1: Int, key2: Int): Unit = {
        // Get swap indexes
        val key1Index = map(key1).remove(0)
        val key2Index = map(key2).remove(0)

        // Swap on map
        map(key1).append(key2Index)
        map(key2).append(key1Index)

        // Swap on index
        val temp = copy(key1Index)
        copy(key1Index) = copy(key2Index)
        copy(key2Index) = temp
      }

      swaps
    }

    val ascSwapCount = swapCount(arr)
    val descSwapCount = swapCount(arr, -1)
    if (ascSwapCount < descSwapCount) ascSwapCount else descSwapCount
  }

  def quicksortHoares(arr: Array[Int]): Int = {
    if (arr == null || arr.isEmpty) return 0
    var swaps = 0

    def swap(i:Int, j:Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    def hoareParition(f:Int, t:Int): Int = {
      var i = f - 1
      var j = t + 1
      val pivot = arr(f)

      scala.util.control.Breaks.breakable {
        while (true) {
          // Find leftmost element greater
          // than or equal to pivot
          do {
            i += 1
          } while (arr(i) < pivot)

          // Find rightmost element smaller
          // than or equal to pivot
          do {
            j -= 1
          } while (arr(j) > pivot)

          // If two pointers met.
          if (i >= j) {
            scala.util.control.Breaks.break
          }

          swap(i, j)
          swaps += 1
        }
      }

      j
    }

    def qs(f:Int, t:Int):Unit = {
      if (f < t) {
        val kth = hoareParition(f, t)
        qs(f, kth)
        qs(kth+1, t)
      }
    }

    qs(0, arr.length-1)

    swaps
  }

  def mergeSortAgain(arr: Array[Int]): Int = {
    if (arr == null || arr.isEmpty) return 0
    var swaps = 0

    def mergeInPlace(f: Int, m: Int, t: Int): Unit = {
      val ALength = m - f + 1 // Because 0 is not a valid size
      val BLength = t - m

      val A = Array.ofDim[Int](ALength)
      val B = Array.ofDim[Int](BLength)

      for (i <- 0 until  ALength) A(i) = arr(f+i)
      for (i <- 0 until BLength) B(i) = arr(m+i+1) // +1 because we don't want to overlap with m

      var k = f
      var p1 = 0
      var p2 = 0
      while (p1 < ALength && p2 < BLength) {
        if (A(p1) < B(p2)) {
          arr(k) = A(p1)
          p1 += 1
        } else {
          swaps += 1
          arr(k) = B(p2)
          p2 += 1
        }
        k += 1
      }

      while (p1 < ALength) {
        arr(k) = A(p1)
        p1 += 1
        k += 1
      }

      while (p2 < BLength) {
        arr(k) = B(p2)
        p2 += 1
        k += 1
      }
    }

    def merge(f:Int, t:Int): Unit = {
      if (f < t) {
        val mid = (f + t) / 2

        merge(f, mid)
        merge(mid + 1, t)

        mergeInPlace(f, mid, t)
      }
    }

    merge(0, arr.length-1)

    println(arr.mkString)

    swaps
  }

  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    if (expenditure == null || expenditure.length <= d) return 0
    val mid = d/2
    val isEven = d%2 == 0
    var warnings = 0

    val buckets = Array.fill[Int](201)(0)
    var count = 0

    for (i <- 0 until d) {
      buckets(expenditure(i)) += 1
    }

    var median1 = 0
    var median2 = 0
    for (i <- d until expenditure.length) {
      count = 0

      // Even
      median1 = 0
      while (count < mid) {
        count += buckets(median1)
        median1 += 1
      }
      median1 -= 1

      // Odd
      median2 = median1+1
      while (count <= mid) {
        count += buckets(median2)
        median2 += 1
      }
      median2 -= 1

      if (isEven) {
        if (median1+median2 <= expenditure(i)) warnings += 1
      } else {
        if (expenditure(i) >= median2*2) warnings += 1
      }

      buckets(expenditure(i-d)) -= 1
      buckets(expenditure(i)) += 1
    }

    warnings
  }

  def insertionSortAnalysis(arr: Array[Int]): Long = {
    // If we use merge sort and count the shifts
    // then we'll be at n log n instead of just
    // using insertion sort
    var shifts = 0

    def merge(s:Int, m:Int, e:Int): Unit = {
      val L_Size = m - s + 1
      val R_Size = e - m
      val L = Array.ofDim[Int](L_Size)
      val R = Array.ofDim[Int](R_Size)

      for (i <- 0 until L_Size) L(i) = arr(s+i)
      for (i <- 0 until R_Size) R(i) = arr(m+i+1)

      var k = s
      var p1 = 0
      var p2 = 0
      while (p1 < L_Size && p2 < R_Size) {
        if (L(p1) > R(p2)) {
          shifts += L_Size-p1
          arr(k) = R(p2)
          p2 += 1
        } else {
          arr(k) = L(p1)
          p1 += 1
        }
        k += 1
      }

//      shifts += p2*(L_Size-p1)

      while (p1 < L_Size) {
        arr(k) = L(p1)
        p1 += 1
        k += 1
      }

      while (p2 < R_Size) {
        arr(k) = R(p2)
        p2 += 1
        k += 1
      }
    }

    def merge_sort(i:Int, j:Int): Unit = {
      if (i < j) {
        val mid = (i+j)/2
        merge_sort(i, mid)
        merge_sort(mid+1, j)
        merge(i, mid, j)
      }
    }

    merge_sort(0, arr.length-1)

    shifts
  }

  def closestNumbers(arr: Array[Int]): Array[Int] = {
    val arr2 = arr.sorted
    val distance = Array.ofDim[Int](arr.length-1)
    var smallest = Integer.MAX_VALUE

    for (i <- 1 until arr2.length) {
      val diff = arr2(i) - arr2(i-1)
      if (diff < smallest) smallest = diff
      distance(i-1) = diff
    }

    println(smallest)

    val result = scala.collection.mutable.ListBuffer[Int]()
    for (i <- 0 until distance.length) {
      if (distance(i) == smallest) {
        result.append(arr2(i))
        result.append(arr2(i+1))
      }
    }

    result.toArray
  }

  def countSort(arr: Array[Array[String]]) {
    val count = List(List[String]())
    val mid = arr.length/2
    for (i <- 0 until arr.length) {
      val input = arr(i)(0).split(" ")
      val value = if (i >= mid) arr(i)(1) else "-"

      count(input(0).toInt)
    }

    for (i <- 0 until 100) count(i).foreach(j => print(s"$j "))
  }

  def countingSort(arr: Array[Int]): Array[Int] = {
    val count = Array.ofDim[Int](100)
    for (i <- 0 until arr.length) {
      count(arr(i)) += 1
    }

    val sort = Array.ofDim[Int](arr.length)
    var sortI = 0
    for (i <- 0 until 100) {
      while (count(i) > 0) {
        sort(sortI) = i
        count(i) -= 1
        sortI += 1
      }
    }

    sort
  }

  def merger_first_into_second(arr1: Array[Int], arr2: Array[Int]): Unit = {
    // Write your code here
    var p1 = arr1.length - 1
    var p2 = p1
    var i = arr2.length - 1
    while (p1 > -1 && p2 > -1) {
      if (arr1(p1) > arr2(p2)) {
        arr2(i) = arr1(p1)
        p1 -= 1
      } else {
        arr2(i) = arr2(p2)
        p2 -= 1
      }
      i -= 1
    }

    while (p1 > -1) {
      arr2(i) = arr1(p1)
      p1 -= 1
      i -= 1
    }

    while (p2 > -1) {
      arr2(i) = arr2(p2)
      p1 -= 1
      i -= 1
    }

    println(arr2.mkString(", "))

  }

  def quickSort2(arr: Array[Int]): Unit = {
    def swap(i:Int, j:Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    def partition(s:Int, e:Int): Int = {
      var p1 = s
      var p2 = s

      swap(e, s)
      while (p1 <= e) {
        if (arr(p1) < arr(s)) {
          p2 += 1
          swap(p1, p2)
        }
        p1 += 1
      }
      swap(p2, s)

      p2
    }

    def qs(s:Int, e:Int): Unit = {
      if (s < e) {
        val kth = partition(s, e)

        qs(s, kth-1)
        qs(kth+1, e)
      }
    }

    qs(0, arr.length - 1)
  }

  def dutch_flag_sort(balls: Array[Char]): Array[Char] = {
    def swap(i:Int, j:Int): Unit = {
      val temp = balls(i)
      balls(i) = balls(j)
      balls(j) = temp
    }

    def three_way_partition(s:Int, e:Int): Unit = {
      var red = s
      var blue = e
      var green = 0
      while (green <= blue) {
        if (balls(green) == 'B') {
          swap(green, blue)
          blue -= 1
        } else if (balls(green) == 'R') {
          swap(green, red)
          red += 1
          green += 1
        } else {
          green += 1
        }
      }
    }

    three_way_partition(0, balls.length - 1)

    balls
  }

  def insertionSort1(n: Int, arr: Array[Int]) {
    for (i <- 0 until arr.length) {
      var p = i - 1
      val copy = arr(i)
      while (p >= 0 && copy < arr(p)) {
        arr(p+1) = arr(p)
        p -= 1
      }
      arr(p+1) = copy
      println(arr.mkString(" "))
    }
  }

  def bigSorting(unsorted: Array[String]): Array[String] = {
    // For sake of practice I'll do quicksort
    def lomuto(s:Int, e:Int):Int = {
      def swap(i:Int, j:Int):Unit = {
        val temp = unsorted(i)
        unsorted(i) = unsorted(j)
        unsorted(j) = temp
      }

      var p1 = s
      swap(s, e)
      for (p2 <- (s+1) to e) {
        if (unsorted(p2).length < unsorted(s).length || (unsorted(p2).length == unsorted(s).length && unsorted(p2).compareTo(unsorted(s)) < 0)) {
          p1 += 1
          swap(p1, p2)
        }
      }
      swap(s, p1)

      p1
    }

    def quickSort(s:Int, e:Int): Unit = {
      if (s < e) {
//        val mid = (s+e)/2
        val ith = lomuto(s, e)
        quickSort(s, ith-1)
        quickSort(ith+1, e)
      }
    }

    quickSort(0, unsorted.length - 1)

    unsorted
  }

  def merge_sort(arr: Array[Int]): Array[Int] = {
    if (arr == null || arr.length < 2) return arr

    def merge(s: Int, m: Int, e: Int): Unit = {
      var i = 0
      var j = 0
      var k = s
      val L_size = m - s + 1
      val R_size = e - m
      val L = Array.ofDim[Int](L_size)
      val R = Array.ofDim[Int](R_size)

      for (i <- 0 until L_size) L(i) = arr(s + i)
      for (j <- 0 until R_size) R(j) = arr(m + 1 + j)

      // Merge both
      i = 0
      j = 0
      k = s
      while (i < L_size && j < R_size) {
        if (L(i) <= R(j)) {
          arr(k) = L(i)
          i += 1
        } else {
          arr(k) = R(j)
          j += 1
        }
        k += 1
      }

      // Fast forward
      while (i < L_size) {
        arr(k) = L(i)
        i += 1
        k += 1
      }

      while (j < R_size) {
        arr(k) = R(j)
        j += 1
        k += 1
      }
    }

    // Merge logic
    def sort(s:Int, e:Int): Unit = {
      if (s < e) {
        val mid = (s + e) / 2

        sort(s, mid)
        sort(mid+1, e)

        merge(s, mid, e)
      }
    }

    sort(0, arr.length - 1)

    arr
  }

  def findZeroSum(arr: Array[Int]): Array[String] = {
    if (arr == null || arr.length == 0) return Array[String]()

    // O(n)
    def twoPointerTechnique(l:Array[Int], r:scala.collection.mutable.Set[String], ith: Int):Unit = {
      var p1 = ith + 1
      var p2 = l.size - 1

      while (p1 < p2) {
        val sum = l(p1) + l(p2) + l(ith)
        if (sum > 0) p2 -= 1
        else if (sum < 0) p1 += 1
        else if (sum == 0) {
          r += (s"${l(ith)}, ${l(p1)}, ${l(p2)}")
          p1 += 1
          p2 -= 1
        }
      }
    }

    // n + (n log n) = O(n log n)
    val unique = arr.sorted

    // n + n
    val result = scala.collection.mutable.Set[String]()
    for (i <- 0 until unique.size) {
      twoPointerTechnique(unique, result, i)
    }

    // O(n^2)
    result.toArray
  }

  def groupNumbersDivisibleBy2OnLeft(arr: Array[Int]): Array[Int] = {
    // In-Place
    var evenPointer = 0
    var oddPointer = 0

    while (evenPointer < arr.length && oddPointer < arr.length) {
      if (isEven(oddPointer)) {
        swap(evenPointer, oddPointer)
        evenPointer += 1
      }

      oddPointer += 1
    }

    def isEven(i:Int):Boolean = arr(i) % 2 == 0

    def swap(i:Int, j:Int): Unit = {
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    arr
  }

  // Merge a bunch of sorted arrays
  def mergeArrays(arr: Array[Array[Int]]): Array[Int] = {
    if (arr == null) return null;

    def getOrder(a:Array[Array[Int]]): Short = {
      var i = 0
      var order:Short = 0
      while (i < a.length) {
        var j = a(i).length - 2
        while (order == 0 && j > -1) {
          if (a(i)(j+1) > a(i)(j)) order = 1
          else if (a(i)(j+1) < a(i)(j)) order = -1
          j -= 1
        }
        i += 1
      }
      order
    }

    // Write your code here
    // O (m + n)
    // Use the logic or merge sort with 2 pointers
    var pointer = 0
    var pointer2 = 0
    var length = 0
    val order:Short = getOrder(arr)

    for (i <- 0 until arr.length) length += arr(i).length
    val result = Array.ofDim[Int](length)

    var continue = true
    while (continue) {
      continue = false

      for (i <- 0 until arr.length) {
        if (pointer < arr(i).length) {
          insert(arr(i)(pointer), pointer2, order)
          pointer2 += 1
          continue = true
        }
      }

      pointer += 1
    }

    def insert(item:Int, p: Int, order: Short):Unit = {
      def swap(i:Int, j:Int):Unit = {
        val temp = result(i)
        result(i) = result(j)
        result(j) = temp
      }

      result(p) = item
      var p1 = p
      var p2 = p - 1
      while (p1 > -1 && p2 > -1 && compare()) {
        swap(p2, p1)
        p1 -= 1
        p2 -= 1
      }

      def compare():Boolean = {
        if (order > 0)
          result(p2) > result(p1)
        else
          result(p2) < result(p1)
      }
    }

    result
  }

  // Can be in place
  // Best and average case is nlog(n)
  // Worst case is O(n^2) because worst case turns into insertion sort
  def quickSort(a:Array[Int]):Unit = {

    def swap(saa:Array[Int], i:Int, j:Int):Unit = {
      val temp = saa(i)
      saa(i) = saa(j)
      saa(j) = temp
    }

    def lomutoPartition(sa:Array[Int], s:Int, e:Int):Int = {
      // Lomuto's algorithm uses pivot point as the end of the array always
      // if you want to use random, swap a random element of the array with end then call Lomutos algorithm
      swap(sa, s, e)
      var p1 = s
      var p2 = s+1
      while (p2 <= e) {
        // Always check with pivot which is in the starting position
        if (sa(p2) < sa(s)) {
          p1 += 1
          swap(sa, p1, p2)
        }
        p2 += 1
      }
      swap(sa, s, p1)

      p1
    }

    def qs(sa:Array[Int], s:Int, e:Int):Unit = {
      if (s < e) {
        // Use Lomuto, ith is a sorted index
        // Hoares goes from both ends of array
        val ith = lomutoPartition(sa, s, e)
        // Left
        qs(sa, s, ith - 1)
        // Right
        qs(sa, ith + 1, e)
      }
    }

    qs(a, 0, a.length-1)

    println(a.mkString(", "))
  }

  // Always nlog(n)
  // Not in place!
  // This is bad
  def mergeSort(arr: Array[Int]): Array[Int] = {
    // Split logic
    def split(a:Array[Int]): (Array[Int], Array[Int]) = {
      val mid = a.length/2
      val left = Array.ofDim[Int](mid)
      val right = Array.ofDim[Int](a.length-mid)
      for (i <- 0 until left.length) left(i) = a(i)
      for (i <- 0 until right.length) right(i) = a(i+mid)
      (left, right)
    }

    // Merge logic
    def merge(l:Array[Int], r:Array[Int]): Array[Int] = {
      val mergedArrays = Array.ofDim[Int](l.length + r.length)
      var lp = 0
      var rp = 0
      var mergedArraysI = 0
      while (lp < l.length && rp < r.length) {
        if (l(lp) > r(rp)) {
          mergedArrays(mergedArraysI) = r(rp)
          rp += 1
        } else {
          mergedArrays(mergedArraysI) = l(lp)
          lp += 1
        }
        mergedArraysI += 1
      }

      // Fast forward left
      while (lp < l.length) {
        mergedArrays(mergedArraysI) = l(lp)
        lp += 1
        mergedArraysI += 1
      }

      // Fast forward right
      while (rp < r.length) {
        mergedArrays(mergedArraysI) = r(rp)
        rp += 1
        mergedArraysI += 1
      }

      mergedArrays
    }

    // Recursion logic
    if (arr == null || arr.length < 2) return arr
    val (left, right) = split(arr)
    val sortedLeft = mergeSort(left)
    val sortedRight = mergeSort(right)
    merge(sortedLeft, sortedRight)
  }

  // O(n) best case
  // O(n^2) worst case
  // Decrease and conquer
  def insertionSort(a: Array[Int]):Unit = {
    def is(sa: Array[Int], n: Int):Unit = {
      if (n == 0) return
      is(sa, n-1)

      val copy = sa(n)
      var j = n-1
      while (j > -1 && sa(j) > copy) {
        sa(j+1) = sa(j)
        j -= 1
      }

      sa(j+1) = copy
    }

    is(a, a.length-1)

    println(a.mkString(", "))
  }

  class ShuffleArray(_nums: Array[Int]) {
    private val random = scala.util.Random

    /** Resets the array to its original configuration and return it. */
    def reset(): Array[Int] = {
      _nums
    }

    /** Returns a random shuffling of the array. */
    def shuffle(): Array[Int] = {
      val a:Array[Int] = Array()
      val b = a ++ _nums

      for (i <- 0 until b.length)
        swap(b, i, random.nextInt(b.length-i)+i)

      b
    }

    private def swap(a:Array[Int], i:Int, j:Int) = {
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }

  }

  def maxProfit2(array: Array[Int]):Int = {
    if (array == null || array.length < 2) return 0
    val s = array.length

    var max = 1
    var min = 1
    for (i <- 1 until s) max = if (array(max) < array(i)) i else max
    for (i <- 0 to max) min = if (array(min) > array(i)) i else min

    val p = if (max < min) 0 else array(max) - array(min)

    /////

    min = 1
    for (i <- 0 until s) min = if (array(min) > array(i)) i else min
    max = min
    for (i <- min+1 until s) max = if (array(max) < array(i)) i else max

    val pp = if (max < min) 0 else array(max) - array(min)

    if (pp > p) pp else p
  }

  def climbStairs(n: Int): Int = {
    // You are climbing a stair case. It takes n steps to reach to the top.
    // Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
    // 0 = 0, 1 = 1, 2 = 2, 3 = 3, 5, 8 = fibonacci

    def fibonacci(n:Int): Int = {
      if (n < 3) return n
      fibonacci(n-1) + fibonacci(n-2)
    }

    fibonacci(n)
  }

  def firstBadVersion(n: Int): Int = {
    // Bad Practice (Does not work)
    // val versions = Array.range(1, (n+1))
    // versions.find(i => isBadVersion(i)).getOrElse(-1)

    def isBadVersion(i:Int) = i >= -33

    def bs(n: Int, f: Int = 1, t: Int = n):Int = {
      val mid = (f + t) / 2

      val check = isBadVersion(mid)
      if (f >= t) if (check) t else -1
      else if (check) bs(n, f, mid)
      else bs(n, mid+1, t)
    }

    bs(n)
  }

  def firstBadVersionBinary(n: Int): Int = {
    // Bad Practice (Does not work)
    // val versions = Array.range(1, (n+1))
    // versions.find(i => isBadVersion(i)).getOrElse(-1)

    def isBadVersion(i:Int) = i >= 1000

    def bs(n: Int, f: Int = 1, t: Int = n):Int = {
      val fc = isBadVersion(f)
      val tc = isBadVersion(t)
      if (fc == tc) if (tc) return f else return -1

      val mid = (f + t) / 2
      if (isBadVersion(mid)) bs(n, f, mid)
      else bs(n, mid+1, t)
    }

    bs(n)
  }

  def bubbleSort(nums: Array[Int]):Unit = {
    def swap(i: Int, j: Int):Unit = {
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }

    for (i <- 0 until nums.length)
      for (j <- 0 until nums.length-i-1)
        if (nums(j) > nums(j+1)) swap(j, j+1)
  }

  def binarySearch(nums: Array[Int], item: Int):Int = {
    def bs(nums: Array[Int], f: Int, t: Int, item: Int):Int = {
      val mid = (f + t) / 2

      val midVal = nums(mid)
      if (item == midVal) return mid
      else if (mid == 0 || mid == nums.size-1) return -1

      if (midVal > item) {
        bs(nums, f, mid-1, item)
      } else {
        bs(nums, mid+1, t, item)
      }
    }

    bs(nums, 0, nums.size-1, item)
  }

  def sortedArrayToBST(nums: Array[Int]): TreeNode = {

    def rec(start: Int, end: Int): TreeNode = {
      if (start > end) null
      else {
        val mid = (start + end) / 2
        new TreeNode(nums(mid), rec(start, mid - 1), rec(mid + 1, end))
      }
    }

    rec(0, nums.size-1)
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    import scala.collection._
    if (root == null) return List()

    case class Item(node: TreeNode, level: Int)
    val queue = mutable.Queue[Item]()

    queue.enqueue(Item(root, 0))

    var lastLevel = 0
    val finalList = mutable.ListBuffer[List[Int]]()
    var tempList = mutable.ListBuffer[Int]()
    while (!queue.isEmpty) {
      val r = queue.dequeue()
      if (r.node != null) {
        // Store the current level and reset
        if (lastLevel != r.level) {
          finalList.append(tempList.toList)
          tempList = mutable.ListBuffer[Int]()
        }
        // Build current level
        tempList.append(r.node.value)
        lastLevel = r.level

        queue.enqueue(Item(r.node.left, r.level + 1))
        queue.enqueue(Item(r.node.right, r.level + 1))
      }
    }

    finalList.append(tempList.toList)

    finalList.toList
  }

  def isSymmetric(root: TreeNode): Boolean = {
    import scala.collection._

    def left(node: TreeNode, sb: StringBuilder = StringBuilder.newBuilder, direction: Int = 0):mutable.StringBuilder = {
      if (node == null) return sb
      sb.append(node.value)
      sb.append(direction)
      left(node.left, sb, 1)
      if (node == root) return sb
      left(node.right, sb, 2)
    }

    def right(node: TreeNode, sb: StringBuilder = StringBuilder.newBuilder, direction: Int = 0):mutable.StringBuilder = {
      if (node == null) return sb
      sb.append(node.value)
      sb.append(direction)
      right(node.right, sb, 1)
      if (node == root) return sb
      right(node.left, sb, 2)
    }

    left(root).mkString.equals(right(root).mkString)
  }

  def isValidBST(root: TreeNode): Boolean = {

    def calculate(root: TreeNode, min: Option[Int], max: Option[Int]): Boolean = {
      if (root == null) true
      else {
        if (min.nonEmpty && root.value <= min.get) false
        else if (max.nonEmpty && root.value >= max.get) false
        else {
          calculate(root.left, min, Some(root.value)) && calculate(root.right, Some(root.value), max)
        }
      }
    }

    calculate(root, None, None)
  }

  def maxDepth(root: TreeNode): Int = {
    if (root == null) return 0

    val leftHeight = maxDepth(root.left)
    val rightHeight = maxDepth(root.right)

    (if (leftHeight > rightHeight) leftHeight else rightHeight) + 1
  }

  def hasCycle(head: ListNode): Boolean = {
    if (head == null) return false

    var one:ListNode = head
    var two:ListNode = head.next

    while (one != null && two != null && one != two) {
      one = one.next
      two = if (two.next != null) two.next.next else null
    }

    one == two
  }

  def isPalindrome(head: ListNode): Boolean = {
    import scala.collection._

    var result = true
    val stack = mutable.Stack[Integer]()

    var next = head
    while (next != null) {
      stack.push(next.x)
      next = next.next;
    }

    next = head
    while (next != null && result) {
      if (stack.pop() != next.x) result = false
      next = next.next;
    }

    result
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    var l1next:ListNode = l1
    var l2next:ListNode = l2
    val head:ListNode = new ListNode(0)
    var hnext:ListNode = head

    while (l1next != null && l2next != null) {
      if (l1next.x < l2next.x) {
        hnext.next = new ListNode(l1next.x)
        hnext = hnext.next
        l1next = l1next.next
      } else {
        hnext.next = new ListNode(l2next.x)
        hnext = hnext.next
        l2next = l2next.next
      }
    }

    var finish = if (l1next == null) l2next else l1next
    while (finish != null) {
      hnext.next = new ListNode(finish.x)
      hnext = hnext.next
      finish = finish.next
    }

    head.next
  }

  def reverseList(head: ListNode): ListNode = {
    var prev: ListNode = null
    var cur: ListNode  = head

    while(cur != null) {
      val tmp = cur.next
      cur.next = prev
      prev = cur
      cur = tmp
    }

    prev
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    var next = head
    var prevPosition:ListNode = null
    var position:ListNode = null
    var size = 1
    while (next != null) {
      if (size - n >= 0) {
        if (position == null) {
          position = head
        } else {
          prevPosition = position
          position = position.next
        }
      }
      next = next.next
      size += 1
    }

    if (prevPosition != null) {
      prevPosition.next = prevPosition.next.next
    } else if (position != null) {
      return position.next
    }

    head
  }

  def deleteNode(node: ListNode): Unit = {
    node.x = node.next.x
    if (node.next.next != null) {
      deleteNode(node.next)
    } else {
      node.next = null
    }
  }

  def longestCommonPrefix(strs: Array[String]): String = {
    if (strs.length == 0) return ""

    var i = 0
    var j = strs.length
    var ans = ""
    while (j == strs.length) {
      j=0
      while (j < strs.length && i < strs(j).length && strs(0)(i) == strs(j)(i)) j+=1
      if (j == strs.length) ans += strs(0)(i)
      i+=1
    }

    ans
  }

  def countAndSay(n: Int): String = {
    if (n < 1) return ""

    var rval = "1"
    for (_ <- 1 until n) {
      rval = say(rval)
    }

    def say(word: String): String = {
      val wl = word.length
      var count = 0
      var rval = ""
      var i = 0
      while (i < wl) {
        count = 0
        while ((i+count) < wl && word(i) == word(i+count)) count+=1
        rval += s"$count${word(i)}"
        i += count
      }
      rval
    }

    rval
  }

  def strStr(haystack: String, needle: String): Int = {
    // You can use indexOf
    val hl = haystack.length
    val nl = needle.length

    if (nl == 0) return 0
    else if (hl < nl) return -1

    var L = 0
    var R = 0
    var rIndex = -1
    var continue = true

    while (L < hl && continue) {
      while (R < nl && (L+R) < hl && haystack.charAt(L+R) == needle.charAt(R)) R+=1
      if (R == nl) {continue = false; rIndex = L}
      else {L+=1; R=0}
    }

    rIndex
  }

  def myAtoi(s: String): Int = {
    val ss = s.trim

    if (ss.length == 0) return 0
    else if (ss.length == 1) return if (ss.charAt(0).isDigit) ss.toInt else 0

    val first = ss.charAt(0)
    val second = ss.charAt(1)
    var sign = 1
    var start = 0

    if (!(first match {
      case '-' | '+' => {if (first == '-') sign = -1; start = 1; second.isDigit}
      case x => if (x.isDigit) true else false
    })) return 0

    var continue = true
    var i = start
    var output = 0l
    while (continue && i < ss.length) {
      val nextChar = ss.charAt(i)
      if (nextChar.isDigit) {
        val temp = output * 10 + nextChar.asDigit
        if (temp > Integer.MAX_VALUE) {output = Integer.MAX_VALUE + (if (sign < 0) 1 else 0); continue = false;}
        else if (temp < Integer.MIN_VALUE) {output = Integer.MIN_VALUE; continue = false;}
        else output = temp
      } else {continue = false}
      i+=1
    }

    output.toInt * sign
  }

  def isPalindrome(s: String): Boolean = {
    val u = s.filter(x => x.isLetterOrDigit).toLowerCase()
    u.equals(u.reverse)
  }

  def isAnagram(s: String, t: String): Boolean = {
    import scala.collection._

    if (s.length != t.length) return false

    // Solution 1
    //    s.toSeq.sorted.unwrap.equals(t.toSeq.sorted.unwrap)

    val map = mutable.Map[Char, Int]()

    t.foreach(c => map.put(c, map.getOrElse(c, 0) + 1))

    s.foreach(c => map.put(c, map.getOrElse(c, 1) - 1))

    map.foreach(x => if (x._2 != 0) return false)

    true
  }

  def firstUniqChar(s: String): Int = {
    import scala.collection._

    val map = mutable.Map[Char, Int]()

    s.foreach(x => map.put(x, map.getOrElse(x, 0) + 1))

    var l = 0
    while (l < s.length && map(s(l)) != 1) l+=1

    if (l == s.length) -1 else l
  }

  // Reverse Integer
  def reverse(x: Int): Int = {
    import scala.collection._

    val list = mutable.ListBuffer[Int]()

    val sign = if (x < 0) -1 else 1
    var n = x * sign

    while (n > 0) {
      list += n % 10
      n = n / 10
    }

    val start:Long = 0
    val rval = list.foldLeft(start){(a, v) => (a * 10) + v} * sign

    if (rval > Integer.MAX_VALUE || rval < Integer.MIN_VALUE) 0 else rval.toInt
  }

  def reverseString(s: Array[Char]): Unit = {
    var L = s.length - 1
    var R = 0

    while (R < L) {
      swap(s, R, L)
      R+=1
      L-=1
    }

    def swap(a:Array[Char], i:Int, j:Int) {
      val temp = a(i)
      a(i) = a(j)
      a(j) = temp
    }
  }

  // TODO: Not done
  def rotate(matrix: Array[Array[Int]]): Unit = {

  }

  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    import scala.collection._

    def validate9x9(board: Array[Array[Char]], cA:(Int, Int), cB:(Int, Int)): Boolean = {
      // Arrays
      val setA = mutable.Set[Char]()
      val setB = mutable.Set[Char]()

      // Validate rows
      for (i <- cA._1 until cA._2) {
        for (j <- cB._1 until cB._2) {
          if (isInvalid(setA, board(i)(j)) || isInvalid(setB, board(j)(i))) return false
        }

        setA.clear()
        setB.clear()
      }

      true
    }

    def validate3x3(board: Array[Array[Char]], cA:(Int, Int), cB:(Int, Int)): Boolean = {
      // Arrays
      val set = mutable.Set[Char]()

      // Validate rows
      for (i <- cA._1 until cA._2) {
        for (j <- cB._1 until cB._2) {
          if (isInvalid(set, board(i)(j))) return false
        }
      }

      true
    }

    def isInvalid(set:mutable.Set[Char], v:Char):Boolean = {
      if (v != '.') {
        if (set.contains(v)) return true
        set.add(v)
      }
      false
    }

    // Validate entire board first
    validate9x9(board, (0,9), (0, 9)) &&
      // Validate top boxes
      validate3x3(board, (0,3), (0,3)) &&
      validate3x3(board, (0,3), (3,6)) &&
      validate3x3(board, (0,3), (6,9)) &&
      // Validate middle boxes
      validate3x3(board, (3,6), (0,3)) &&
      validate3x3(board, (3,6), (3,6)) &&
      validate3x3(board, (3,6), (6,9)) &&
      // Validate bottom boxes
      validate3x3(board, (6,9), (0,3)) &&
      validate3x3(board, (6,9), (3,6)) &&
      validate3x3(board, (6,9), (6,9))
  }

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val rval = Array(-1, -1)

    for (i <- 0 until nums.length) {
      for (j <- (i+1) until nums.length) {
        if (nums(i) + nums(j) == target) {
          rval(0) = i
          rval(1) = j
        }
      }
    }

    rval
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    var L = 0
    var R = 0

    while (R < nums.length) {
      if (nums(L) == 0) {
        if (nums(R) != 0) {
          swap(nums, L, R)
          L += 1
        }
        // Do not move L since we're looking for the next non-zero R item
      } else L += 1

      R += 1
    }

    def swap(nums:Array[Int], i:Int, j:Int): Unit = {
      val temp = nums(i)
      nums(i) = nums(j)
      nums(j) = temp
    }
  }

  def plusOne(digits: Array[Int]): Array[Int] = {
    var i = digits.length - 1
    var carry = 1

    while (i >= 0 && carry != 0) {
      if (digits(i) != 9) {
        digits(i) += carry
        carry = 0
      } else {
        digits(i) = 0
      }
      i-=1
    }

    if (carry != 0) 1 +: digits else digits
  }

  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    import scala.collection._

    val largeTuple = if (nums1.length > nums1.length)
      (nums1, nums2) else
      (nums2, nums1)

    val occ = mutable.Map[Int, Int]()

    largeTuple._1.foreach(e => occ += (e -> (occ.getOrElse(e, 0) + 1)))

    largeTuple._2.filter(e => {
      if (occ.getOrElse(e, 0) > 0) {
        occ(e) -= 1
        true
      } else false
    })
  }

  def singleNumber(nums: Array[Int]): Int = nums.reduce(_ ^ _)

  def containsDuplicate(nums: Array[Int]): Boolean = {
    nums.toSet.size != nums.length
  }

  def rotate(nums: Array[Int], k: Int): Unit = {
    import scala.collection._

    if (nums.length < 2) return

    val queue = mutable.Queue[Int]()
    val mod = nums.length - (k % nums.length)

    for (i <- mod until nums.length) {
      queue.enqueue(nums(i))
    }

    for (i <- 0 until mod) {
      queue.enqueue(nums(i))
    }

    for (i <- 0 until nums.length) {
      nums(i) = queue.dequeue()
    }
  }

  def maxProfit(prices: Array[Int]): Int = {
    var profit = 0

    (1 until prices.size).foreach{ day =>
      if (prices(day) > prices(day-1)) {
        profit += prices(day) - prices(day-1)
      }
    }

    profit
  }

  def removeDuplicates(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      var x = 0
      for (y <- 1 until nums.length) {
        if (nums(x) != nums(y)) {
          x = x + 1
          nums(x) = nums(y)
        }
      }
      x+1
    } else 0
  }

}