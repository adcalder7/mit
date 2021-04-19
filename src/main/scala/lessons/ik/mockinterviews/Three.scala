package lessons.ik.mockinterviews

object Three {

/* Computing Team Quality
 *
 * When assembling a team of workers to perform a task, we focus on
 * professionalism and speed of the workers. To Calculate the overall quality of a team
 * of workers, we take the sum of each worker's speed, and multiply it by the lowest
 * professionalism rating of all the workers.
 *
 * Given information about several available workers, select workers to create a team of
 * less than or equal to a particular size. Determine the maximum quality of the team
 * that can be created.
 *
 * Example 1
 * speed           = [4,3,15,5,6]
 * professionalism = [7,6, 1,2,8]
 * maxWorkers      = 3
 *
 * The maximum number of workers go use is maxWorkers = 3 chosen from 5 available workers.
 * A worker[i]'s speed and professionalism rating are speed[i] and professionalism[i].
 *
 * Select the first, second, and fifth workers. The maximum quality of the team is:
 *    (speed[0]+speed[1]+speed[4]) * min(professionalism[0],professionalism[1],professionalism[4])=
 *    (4+3+6) * min(7,6,8) = 13 * 6 = 78
 *
 * Example 2
 * speed           = [12,112,100,13,55]    [100,13,55,12,112]
 * professionalism = [31,  4,100,55,50] => [100,55,50,31,100]

K = 100000

6, 4, 3, 5, 15
8, 7, 6, 2, 1

   A (6*8)
   B (6+4) * 7
   C (6+4+3) * 6
   D (5 + Heap(64,63,43) * 2)

   NlogN + NlogN

 */
}

object Solution22 {
  def maxPerformance(n: Int, speed: Array[Int], efficiency: Array[Int], k: Int): Int = {
    import collection.mutable.{PriorityQueue, Buffer}

    /*
    You have to drop the lowest of the sums because efficiency will only get wrost
    since we're sort efficiency descending. So we can keep the windows with
    max performances by dropping the lowest of the sums
    */

    case class Engineer(s:Int, e:Int)

    // N - Combine info
    var el = Buffer[Engineer]()
    for (i <- 0 until speed.size) {
      el.append(Engineer(speed(i), efficiency(i)))
    }

    // NlogN - Sort efficiency
    val els = el.sorted(Ordering.by((e:Engineer) => -e.e))

    // NlogN
    val queue = PriorityQueue.empty[Long](Ordering.by((i:Long) => -i))
    var perf = 0l
    var sum = 0l
    for (e <- els) {
      if (queue.size == k) {
        // Drop the lowest sum so our performance window stays relatively high
        sum -= queue.dequeue()
      }
      // The trick here is that it does not matter if the peformance goes down
      // We are capturing the max outside with global variable
      // This is just a sliding window giving you the best possible performances
      // It is up to you to store a record of the best
      queue.enqueue(e.s)
      sum += e.s
      perf = Math.max(perf, sum * e.e)
    }

    (perf % 1000000007).toInt
  }
}
