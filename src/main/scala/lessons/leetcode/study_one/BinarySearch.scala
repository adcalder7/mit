package lessons.leetcode.study_one

object BinarySearch extends App {

  var a = Array(1,2,3,4,5,6,7,8,9,10)

  // Using binary search
  def indexOf(x:Int, b:Array[Int]):Int = {
    var l = 0
    var r = b.size - 1 // Since 0 is a position, 0 - 9 is the true range

    while (l <= r) {
      val midIndex = l + (r - l) / 2 // True middle
      val midValue = b(midIndex)

      if (midValue > x) r = midIndex - 1 // Exclude mid since it has been observed
      else if (midValue < x) l = midIndex + 1 // Exclude mid since it has been observed
      else return midIndex
    }

    -1
  }

  println(indexOf(78, a))
}
