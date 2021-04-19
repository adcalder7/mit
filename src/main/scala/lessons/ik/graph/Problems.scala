package lessons.ik.graph

import scala.collection.mutable.ArrayBuffer


object Problems extends App {

  class Node(_value: Int) {
    val value: Int = _value
    val neighbours = ArrayBuffer[Node]()
  }

  val s = new Node(1)
  s.neighbours.append(new Node(2))
  build_other_graph(s)
  def build_other_graph(node: Node): Node = {
    import collection.mutable.{Map, Queue, Buffer}
    // Build reverse graph
    val AJL = Map[Int, Node]()
    val reversedAJL = Map[Int, Node]()

    val f = Buffer[Char]()

    // Build ajl
    val queue = Queue[Node](node)
    while (!queue.isEmpty) {
      val node = queue.dequeue()

      AJL.put(node.value, node)
      reversedAJL.getOrElseUpdate(node.value, new Node(node.value))

      node.neighbours.foreach(child => {
        if (!AJL.contains(child.value)) {
          queue.enqueue(child)
        }
      })
    }

    AJL.foreach(p => {
      p._2.neighbours.foreach(n => {
        reversedAJL(n.value)
          .neighbours
          .append(reversedAJL.getOrElseUpdate(p._1, new Node(p._1)))
      })
    })

    reversedAJL(node.value)
  }

  println(find_order(Array("abc","ab")))
  def find_order(words: Array[String]): String = {
    import collection.mutable.{Map, Buffer}

    // Build Adjacency list
    val al = Map[Char, Buffer[Char]]()
    words.foreach(_.foreach(c => al.getOrElseUpdate(c, Buffer())))

    var m = 0
    for (i <- 1 until words.length) {
      val A = words(i-1)
      val B = words(i)

      // Check that word2 is not a prefix of word1.
      if (A.length() > B.length() && A.startsWith(B)) return ""

      var cont = true
      m = 0
      val size = Math.min(A.length, B.length)
      while (cont && m < size) {
        if (A(m) != B(m)) {
          al(A(m)).append(B(m))
          cont = false
        }
        m += 1
      }
    }

    // Topological sort the AJ
    // Topological Order only works with Stack (Post traversal)
    val seen = Map[Char, Boolean]()
    val result = Buffer[Char]()
    def DFS(node:Char):Boolean = {
      if (seen.contains(node)) {
        seen(node)
      } else {
        seen.put(node, false)
        if (al.contains(node)) {
          al(node).foreach(dependency => if (!DFS(dependency)) {
            return false
          })
        }
        seen.put(node, true)
        result.prepend(node)
        true
      }
    }

    al.foreach(n => if (!DFS(n._1)) return "")

    if (result.size == al.size) result.mkString else ""
  }

//  find_critical_connections(2,
//    Array(Array(1,0), Array(1,0))).foreach(e => println(e.mkString(",")))
  def find_critical_connections(number_of_servers: Int,
                                connections: Array[Array[Int]]): Array[Array[Int]] = {
    import collection.mutable.{ListBuffer, Set, Map}

    val output = ListBuffer[Array[Int]]()

    // Build adjacency list
    val al = Map[Int, Set[Int]]()
    connections.foreach(e => {
      al.getOrElseUpdate(e(1), Set()).add(e(0))
      al.getOrElseUpdate(e(0), Set()).add(e(1))
    })

    // Find bridges
    val visited = Set[Int]()
    val disc = Array.ofDim[Int](number_of_servers)
    // Low = strongly connected components grouped by value
    val low = Array.ofDim[Int](number_of_servers)
    var timer = 0
    def DFS(child:Int, parent:Int): Unit = {
      // Mark as visited
      visited.add(child)

      // Continuously assign discovery and low for new node
      disc(child) = timer
      low(child) = timer
      timer += 1

      // For each child
      al(child).foreach(nextChild => {
        // We don't perform logic on the parent where we came from
        if (parent != nextChild) {
          if (!visited.contains(nextChild)) {
            DFS(nextChild, child)
            // Because this will get executed after the else
            // then our child may or may not have a lower link than current root
            // if it does then this is a cycle, if not then we have a bridge
            if (disc(child) < low(nextChild)) output.append(Array(child, nextChild))
            low(child) = Math.min(low(child), low(nextChild))
          } else {
            // This is a way to identify back edges in DFS
            // If this child is not root's parents (the way we came)
            // Then we have a back-edge because child is another way
            // to get to root
            // update root with this child's low-link value
            // Basically propagate low value of this current cycle
            low(child) = Math.min(low(child), disc(nextChild))
          }
        }
      })
    }

    if (al.contains(0)) DFS(0, -1)
    if (output.isEmpty) Array(Array(-1,-1)) else output.toArray
  }

//  println(course_schedule(5, Array(Array(3,1))).mkString(","))
  def course_schedule(n: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
    import collection.mutable.{Map, Set, ListBuffer}

    // Build adjacency list
    val al = Map[Int, ListBuffer[Int]]()
    prerequisites.foreach(p => {
      al.getOrElseUpdate(p(1), ListBuffer()).append(p(0))
    })

    // Topological Order only works with Stack (Post traversal)
    val visited = Set[Int]()
    val result = ListBuffer[Int]()
    val cycleWatch:Set[Int] = Set()

    // Returns true if there is a cycle
    def DFS(node:Int):Boolean = {
      visited.add(node)
      cycleWatch.add(node)
      if (al.contains(node)) {
        al(node).foreach(dependency => {
          if (cycleWatch.contains(dependency)) return true
          if (!visited.contains(dependency) && DFS(dependency)) return true
        })
      }
      cycleWatch.remove(node)

      // Cheating for the topological order
      if (result.size < n) result.prepend(node)

      false
    }

    for {i <- 0 until n} {
      if (!visited.contains(i) && DFS(i)) return Array(-1)
    }

    result.toArray
  }

//  println(zombieCluster(Array("101","010","101")))
  def zombieCluster(zombies: Array[String]): Int = {
    import collection.mutable.{Map, Set, Queue, ListBuffer}
    // Build adjacency list
    // Build BFS
    // Get parents tree
    // Traverse parents tree marking each parents as visited

    val al = ListBuffer[ListBuffer[Int]]()
    for (i <- 0 until zombies.length) {
      al.append(ListBuffer())
      for (j <- 0 until zombies(i).length) {
        if (zombies(i).charAt(j).equals('1')) {
          al(i).append(j)
        }
      }
    }

    def BFS(start:Int, markAsVisited:Set[Int]):Map[Int, Set[Int]] = {
      val queue = Queue[Int](start)
      val visited = Set[Int]()
      val parent = Map[Int, Set[Int]]()

      while (!queue.isEmpty) {
        val node = queue.dequeue()
        visited.add(node)

        al(node).foreach(child => {
          if (!visited.contains(child)) {
            visited.add(child)
            parent.getOrElseUpdate(node, Set()).add(child)
            queue.enqueue(child)
          }
        })
      }

      visited.foreach(markAsVisited.add)
      parent
    }

    var components = 0
    val visited = Set[Int]()
    for (i <- 0 until al.length) {
      if (!visited.contains(i)) {
        BFS(i, visited)
        components += 1
      }
    }

    components
  }

  // Keys and Doors
//  find_shortest_path(Array(
//    "..............",
//    ".a##########B.",
//    ".#..........#.",
//    ".#.########.#.",
//    ".#.#......#.#.",
//    ".#.#.a##B.#.#.",
//    ".#.#.#.@#.#.#.",
//    ".C.#.D.#ad#.#.",
//    ".#.B.#....#.#.",
//    ".#.#.#####A.#.",
//    ".###........#.",
//    ".#.bA########.",
//    "+#..c.........")).foreach(e => println(e.mkString(",")))
  def find_shortest_path(grid: Array[String]): Array[Array[Int]] = {
    import collection.immutable.{Map, Set, List}

    case class Step(x:Int, y:Int, z:Set[Char]) {
      def equalsIgnoreItems(step:Step):Boolean = x == step.x && y == step.y
    }

    // The way BFS behaves is like cloning a person in a maze
    // If a person is at a point, that person clones himself
    // and sends his clones to different directions
    // Each clone have a set of items

    // BFS supports backtrack by adding a state to the coordinates
    // If we haven't visited a point with the state we have then visit it
    // even if we visited before with another state
    def BFS(start:Step, end:Step):Map[Step,Step] = {
      val queue = collection.mutable.Queue[Step](start)
      val visited = collection.mutable.Set[Step](start)
      val parent = collection.mutable.Map[Step,Step]()

      while (!queue.isEmpty) {
        val step = queue.dequeue()
        if (step.equalsIgnoreItems(end)) {
          // We need to add this step so we know from where to
          // build the path... so there will be 2 ends
          // one with many items collected
          // the one we defined
          parent.put(end, step)
          queue.clear()
        } else {
          getNextSteps(step).filterNot(visited).foreach(nearStep => {
            visited.add(nearStep)
            parent.put(nearStep, step)
            queue.enqueue(nearStep)
          })
        }
      }
      visited.clear()

      parent.toMap
    }

    def getNextSteps(step:Step):List[Step] = {
      def move(x:Int, y:Int, zs:Set[Char]):Step = {
        if (x > -1 && x < grid.length && y > -1 && y < grid(x).length) {
          val z = grid(x)(y)
          if (z == '.' || z == '@' || z == '+') {
            Step(x, y, zs)
          } else if (z.isLower || zs.contains(z.toLower)) {
            Step(x, y, zs + z)
          } else null
        } else null
      }

      List(Step(step.x-1, step.y, step.z),
        Step(step.x+1, step.y, step.z),
        Step(step.x, step.y-1, step.z),
        Step(step.x, step.y+1, step.z)
      ).map(e => move(e.x, e.y, e.z))
        .filter(e => e != null)
    }

    def getShortestPath():Array[Array[Int]] = {
      def getStartAndEnd():(Step,Step) = {
        import scala.util.control.Breaks.{breakable, break}
        var start:Step = null
        var end:Step = null
        breakable {
          for (i <- 0 until grid.size) {
            if (start != null && end != null) break()
            for (j <- 0 until grid(i).length) {
              val room = grid(i)(j)
              if (room == '@') start = Step(i, j, Set())
              if (room == '+') end = Step(i, j, Set())
              if (start != null && end != null) break()
            }
          }
        }
        (start, end)
      }

      val (start, end) = getStartAndEnd()
      val edges = BFS(start, end)

      val output = collection.mutable.Buffer[Array[Int]]()
      var next:Option[Step] = edges.get(end)
      while (next.isDefined) {
        // Don't forget to use prepend since we're walking backwards
        output.prepend(Array(next.get.x, next.get.y))
        next = edges.get(next.get)
      }

      output.toArray
    }

    getShortestPath()
  }

  // BFS, build a parents Map
  // Reconstruct map from end to start since the list will yield shortest with BFS
//  println(string_transformation(Array("cat","bxb","bad"), "bbb", "bxa").mkString(","))
  def string_transformation(choices: Array[String], start: String, stop: String): Array[String] = {

    def BFS():collection.mutable.Map[String, String] = {
      val parent = collection.mutable.Map[String, String]()
      val queue = collection.mutable.Queue[String](start)
      val visited = collection.mutable.Set[String](start)

      // BFS
      while (!queue.isEmpty) {
        val step = queue.dequeue()
        if (differsByOne(stop, step)) {
          parent.put(stop, step)
          queue.clear()
        } else {
          choices.foreach(choice => {
            if (!visited.contains(choice) && differsByOne(step, choice)) {
              queue.enqueue(choice)
              parent.put(choice, step)
              visited.add(choice)
            }
          })
        }
      }
      queue.clear()

      def differsByOne(one: String, two: String): Boolean = {
        if (one.size == two.size) {
          (for {
            i <- 0 until one.size
          } yield {
            if (one(i) != two(i)) 1 else 0
          }).reduceLeft((e, v) => v + e) == 1
        } else false
      }

      parent
    }

    val result = collection.mutable.Buffer[String]()

    val parent = BFS()
    var next = parent.get(stop)
    while (next.isDefined) {
      result.prepend(next.get)
      next = parent.get(next.get)
    }

    if (result.isEmpty)
      result.append("-1")
    else
      result.append(stop)

    result.toArray
  }

  //  println(find_minimum_number_of_moves(5,5,0,0,4,1))
  def find_minimum_number_of_moves(rows: Int,
                                   cols: Int,
                                   start_row: Int,
                                   start_col: Int,
                                   end_row: Int,
                                   end_col: Int): Int = {
    val visited = collection.mutable.Set[(Int, Int, Int)]()
    val queue = collection.mutable.Queue[(Int, Int, Int)]((start_row, start_col, 0))
    var minimum = -1

    while (!queue.isEmpty) {
      val choice = queue.dequeue()
      if (isOnTarget(choice)) {
        minimum = choice._3
        queue.clear()
      } else {
        val choices = getValidChoices(choice)
        visited.addAll(choices)
        queue.enqueueAll(choices)
      }
    }

    def getValidChoices(position: (Int, Int, Int)): collection.immutable.List[(Int, Int, Int)] = {
      // i +- 1, j +- 2
      // i +- 2, j +- 1
      collection.immutable.List[(Int, Int, Int)](
        (position._1 + 1, position._2 + 2, position._3 + 1),
        (position._1 + 1, position._2 - 2, position._3 + 1),
        (position._1 - 1, position._2 + 2, position._3 + 1),
        (position._1 - 1, position._2 - 2, position._3 + 1),
        (position._1 + 2, position._2 + 1, position._3 + 1),
        (position._1 + 2, position._2 - 1, position._3 + 1),
        (position._1 - 2, position._2 + 1, position._3 + 1),
        (position._1 - 2, position._2 - 1, position._3 + 1)).filter(validChoice).filterNot(visited)
    }

    def validChoice(attempt: (Int, Int, Int)): Boolean = {
      !(attempt._1 < 0 || attempt._2 < 0 ||
        attempt._1 > rows - 1 || attempt._2 > cols - 1 ||
        visited.contains(attempt))
    }

    def isOnTarget(choice: (Int, Int, Int)): Boolean = {
      choice._1 == end_row && choice._2 == end_col
    }

    minimum
  }

}
