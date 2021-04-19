package lessons.leetcode.adhoc



object Result extends App {

  class LRU(capacity:Int) {
    import collection.mutable.{Map, SortedSet}

    case class Node(var timestamp:Long, key:Int, var value:Int)
    private var currentTimestamp:Long = 0

    private val freq = SortedSet[Node]()(Ordering.by((i:Node) => i.timestamp)) // (Timestamp, Key)
    private val cache = Map[Int, Node]() // Key, Node(timestamp, value)

    def get(key:Int):Int = {
      if (cache.contains(key)) {
        val ts = getTimestamp()

        val node = cache(key)
        freq.remove(node)
        node.timestamp = ts
        freq.add(node)

        node.value
      } else -1
    }

    def set(key:Int, value:Int):Unit = {
      val ts = getTimestamp()

      if (cache.contains(key)) {
        val node = cache(key)

        freq.remove(node)
        node.value = value
        node.timestamp = ts
        freq.add(node)
      } else {
        val node = Node(ts, key, value)

        if (cache.size == capacity) {
          val minTSNode = freq.head
          freq.remove(minTSNode)
          cache.remove(minTSNode.key)
        }

        cache.put(key, node)
        freq.add(node)
      }
    }

    private def getTimestamp():Long = {
      currentTimestamp += 1
      currentTimestamp
    }
  }

  implement_LRU_cache(2, Array(1,1,0,0), Array(1,2,1,2), Array(5,6,1,1))
  def implement_LRU_cache(capacity: Int, query_type: Array[Int], key: Array[Int], value: Array[Int]): Array[Int] = {
    import collection.mutable.Buffer

    val lru = new LRU(capacity)

    val output = Buffer[Int]()
    for (i <- 0 until query_type.size) {
      if (query_type(i) == 1) {
        // SET
        lru.set(key(i), value(i))
      } else if (query_type(i) == 0) {
        // GET
        output.append(lru.get(key(i)))
      }
    }

    println(output.mkString(", "))

    output.toArray
  }


}


