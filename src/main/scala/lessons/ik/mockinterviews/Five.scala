package lessons.ik.mockinterviews

object InterviewSolution extends App {

  // Histogram, Area

  // Use a monotonically increasing stack
  def largestRectangleArea(heights: Array[Int]): Int = {
    import collection.mutable.Stack

    case class Item(i:Int, h:Int)
    val stack = Stack[Item]()
    var globalMax = 0

    for (i <- 0 until heights.size) {
      val leftI = slideWindow(i, {!stack.isEmpty && stack.top.h > heights(i)})
      stack.push(Item(leftI, heights(i)))
    }

    def slideWindow(i:Int = heights.size, check: => Boolean = {true}):Int = {
      var leftI = i

      while (check) {
        val building = stack.pop()
        val width = i - building.i
        val height = building.h
        globalMax = Math.max(globalMax, width * height)

        //
        leftI = building.i
      }

      leftI
    }

    slideWindow()

    globalMax
  }

}