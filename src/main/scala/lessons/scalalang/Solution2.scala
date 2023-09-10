package lessons.scalalang

object Solution2 extends App {
  import collection.mutable.{Queue, PriorityQueue, Stack}

  var int:Int = 0
  var float:Float = 0
  var double:Double = 0
  var str:String = ""

  var array:Array[Int] = Array[Int](1,2,3)
  var array2:Array[Int] = Array.ofDim[Int](3)

  case class Node(a:Int) {
    var next:Node = null
  }

  var ll:Node = Node(1)
  ll.next = Node(2)

  var map = Map("x" -> 10)
  for (i <- 0 until 10 by 1) println(i)

  var queue = Queue[Int]()
  var stack = Stack[Int]()
  var priorityQueue = PriorityQueue.empty[Int](Ordering.by(i => i))

  println(map)
}