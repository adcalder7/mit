package lessons.ik.graph

import collection.mutable.{Map, Set, Queue, Stack}

class Graph[T] {

  private val map = Map[T, Set[T]]()

  def add(vertex:T, edge:T, biDirectional:Boolean = true):Unit = {
    map.getOrElseUpdate(vertex, Set[T]()).add(edge)
    if (biDirectional) map.getOrElseUpdate(edge, Set[T]()).add(vertex)
  }

  def get(vertex:T):Option[Set[T]] = {
    map.get(vertex)
  }

  def containsVertex(vertex:T, otherVertex:T):Boolean = {
    val v = map.get(vertex)
    v.isDefined && v.get.contains(otherVertex)
  }

  // O(V)
  def remove(vertex:T):Unit = {
    map.remove(vertex)
    map.foreach(e => e._2.remove(vertex))
  }

  def hasEulerianCycle:Boolean = {
    var isCycle = true
    util.control.Breaks.breakable {
      map.foreach(e => {
        if (e._2.size % 2 != 0) {
          isCycle = false
          util.control.Breaks.break
        }
      })
    }
    isCycle
  }

  def hasEulerianPath:Boolean = {
    var oddCount = 0
    util.control.Breaks.breakable {
      map.foreach(e => {
        if (e._2.size % 2 != 0) {
          oddCount += 1
          if (oddCount > 2) util.control.Breaks.break
        }
      })
    }
    oddCount == 2
  }

  def isConnectedHasECycleEPath():(Boolean, Boolean, Boolean) = {
    val visited = Set[T]()
    var oddCount = 0

    val queue = Queue[T]()
    queue.enqueue(map.last._1)

    while (!queue.isEmpty) {
      val vertex = queue.dequeue()
      if (map.contains(vertex)) {
        val children = map(vertex)
        if (children.size % 2 != 0) oddCount += 1
        queue.enqueueAll(map(vertex).filterNot(visited))
      }
      visited.add(vertex)
    }

    (visited.size == map.size, oddCount == 0, oddCount == 2)
  }

  def isConnected() = {
    val visited = Set[T]()

    val queue = Queue[T]()
    queue.enqueue(map.last._1)

    while (!queue.isEmpty) {
      val vertex = queue.dequeue()
      if (map.contains(vertex)) {
        queue.enqueueAll(map(vertex).filterNot(visited))
      }
      visited.add(vertex)
    }

    visited.size == map.size
  }

  // DFS
  def BFS(startVertex: T):Set[T] = {
    val visited = Set[T]()

    val queue = Queue[T]()
    queue.enqueue(startVertex)

    while (!queue.isEmpty) {
      val vertex = queue.dequeue()
      // Keep reference of fringe for better decision making
      val fringeEdges = map(vertex).filterNot(visited)
      visited.addAll(fringeEdges)
      queue.enqueueAll(fringeEdges)
    }

    visited
  }

  // DFS - Stack - Iterative
  def DFS(startVertex: T):Set[T] = {
    val visited = Set[T]()
    val stack = Stack[T]()
    stack.push(startVertex)

    while (!stack.isEmpty) {
      val vertex = stack.pop()
      stack.pushAll(map(vertex).filterNot(visited))
      visited.add(vertex)
    }

    visited
  }

  def DFSRecursive(startVertex: T):Set[T] = {
    val visited = Set[T]()

    def permute(temp: T): Unit = {
      if (!visited.contains(temp) && map.contains(temp)) {
        visited.add(temp)
        map(temp).foreach(permute)
      }
    }

    permute(startVertex)
    visited
  }

  def hasCycle(startVertex: T):Boolean = {
    val visited = Set[T]()

    def permute(root: T):Boolean = {
      visited.add(root)
      var foundCycle = false
      util.control.Breaks.breakable {
        map(root).foreach(vertex => {
          if (visited.contains(vertex) || foundCycle) {
            foundCycle = true
            util.control.Breaks.break()
          } else {
            foundCycle = permute(vertex)
          }
        })
      }
      foundCycle
    }

    permute(startVertex)
  }

  def numberOfGraphComponents():Int = {
    val visited = Set[T]()
    var componentCount = 0

    map.foreach(e => {
      if (!visited.contains(e._1)) {
        visited.add(e._1)

        visited.filterNot(e._2).foreach(vertex => {
          visited.addAll(DFSRecursive(vertex))
        })

        componentCount += 1
      }
    })

    componentCount
  }
}

object Lesson extends App {

  val graph = new Graph[Int]()
  graph.add(1, 2)
  graph.add(2, 3)
  graph.add(3, 1)
//  graph.add(5, 0, false)

  println(graph.hasEulerianCycle)
  println(graph.hasEulerianPath)
  println(graph.isConnected())
  println(graph.hasCycle(1))

  println(graph.BFS(2).mkString(","))
  println(graph.DFS(2).mkString(","))
  println(graph.DFSRecursive(2).mkString(","))
  println(graph.numberOfGraphComponents)
  println(graph.isConnectedHasECycleEPath)

}
