// Divide and Conquer
object Lecture1 {

  // Solve the problem
  // Check functionality
  // Check edge cases
  // Not checking for null to avoid code littering

  // Using binary search to implement indexOf assuming the array is already sorted
  def indexOf(array:Array[Int], value:Int, fromIndex:Int, toIndex:Int):Int = {
    val half = (fromIndex+toIndex) >>> 1 // Also / 2
    val item = array(half)

    if (item == value) half
    else if (half == 0) -1
    else if (item < value) indexOf(array, value, 0, half)
    else indexOf(array, value, 0, half)
  }

  // Find peak using divide and conquer which avoids greedy approach
  def findPeak(array:Array[Int], fromIndex:Int, toIndex:Int):Int = {
    val half = (toIndex+fromIndex) >>> 1
    val item = array(half)

    // Check Left and right and choose direction
    if (half == 0 || half == (toIndex-1)) {
      // If its the beginning or end of the array return as peak
      item
    } else if (array(half-1) > item) {
      // Go left
      findPeak(array, 0, half)
    } else {
      // Go right
      findPeak(array, half, toIndex)
    }
  }

  def main(args:Array[String]): Unit = {
    val numbers = Array(1,2,9,4,5,6)

    // Binary search
    println("Binary Search: ")
    println(indexOf(numbers, 1, 0, numbers.length))

    // Finding a peak
    println("Finding a Peak: ")
    println(findPeak(numbers, 0, numbers.length))
  }

}
