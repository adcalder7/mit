package lessons.leetcode

// Divide and Conquer
object Lecture1 {
  // Solve the problem
  // Check functionality
  // Check edge cases
  // Not checking for null to avoid code littering

  // Using binary search to implement indexOf assuming the array is already sorted O(ln(n))
  def indexOf(array: Array[Int], value: Int, fromIndex: Int, toIndex: Int): Int = {
    val half = (fromIndex + toIndex) >>> 1 // Division by 2
    val item = array(half)

    if (item == value) half
    else if (half == 0) -1
    else if (item < value) indexOf(array, value, 0, half)
    else indexOf(array, value, 0, half)
  }

  // Find peak using divide and conquer which avoids greedy approach O(ln(n))
  def findPeak(array: Array[Int], fromIndex: Int, toIndex: Int): Int = {
    val half = (toIndex + fromIndex) >>> 1
    val item = array(half)

    // Check Left and right and choose direction
    if (half == 0 || half == (toIndex - 1)) {
      // If its the beginning or end of the array return as peak
      item
    } else if (array(half - 1) > item) {
      // Go left
      findPeak(array, 0, half)
    } else {
      // Go right
      findPeak(array, half, toIndex)
    }
  }

  // Find peak using divide and conquer on a 2D array O(h*ln(w))
  def findPeak2D(array: Array[Array[Int]], fromIndex: Int, toIndex: Int): Int = {
    val halfCols = (toIndex + fromIndex) >>> 1

    // O(n) global max in column
    // This can be replaced with divide and conquer which should be O(ln(n)) without wasting space.
    // The complexity lies on passing the column without making a copy of it.
    var (rowIndex, colPeakIndex) = (0, 0)
    array.view.zipWithIndex foreach {
      case (subArray, index) => {
        if (array(rowIndex)(halfCols) < subArray(halfCols)) {
          colPeakIndex = halfCols
          rowIndex = index
        }
      }
    }

    // We don't check of up or bottom because we're finding the pick of the column above
    val peak = array(rowIndex)(colPeakIndex)
    val left = array(rowIndex)(if (halfCols == fromIndex) halfCols else halfCols - 1)
    val right = array(rowIndex)(if (halfCols == toIndex - 1) halfCols else halfCols + 1)

    if (peak < left) {
      findPeak2D(array, 0, halfCols)
    } else if (peak < right) {
      findPeak2D(array, halfCols, array.length)
    } else {
      peak
    }
  }

  def main(args: Array[String]): Unit = {
    val numbers = Array(1, 2, 9, 4, 5, 6)

    val numbers2D = Array(
      Array(10, 8, 10, 10),
      Array(77, 13, 12, 50),
      Array(15, 9, 1, 21),
      Array(16, 17, 19, 20))

    // Finding a peak on a 2D array
    println("Finding a peak on a 2D array: ")
    println(findPeak2D(numbers2D, 0, numbers2D(0).length))

    // Finding a peak on an array
    println("Finding a peak: ")
    println(findPeak(numbers, 0, numbers.length))

    // Binary search
    println("Binary Search: ")
    println(indexOf(numbers, 1, 0, numbers.length))
  }

}
