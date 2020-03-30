// Divide and Conquer
object Lecture1 {

  // Solve the problem
  // Check functionality
  // Check edge cases

  // Using binary search for index of on an array
  // Array needs to be sorted
  def indexOf(array:Array[Int], value:Int, fromIndex:Int, toIndex:Int):Int = {
    val half = (fromIndex+toIndex) >>> 1 // Also / 2
    val item = array(half)

    if (item == value) half
    else if (half == 0) -1
    else if (item < value) indexOf(array, value, 0, half)
    else indexOf(array, value, 0, half)
  }

  def main(args:Array[String]): Unit = {
    val nums = Array(6,7,4,3,2,1,4,5)
    println(indexOf(nums, 1, 0, nums.length))
  }

}
