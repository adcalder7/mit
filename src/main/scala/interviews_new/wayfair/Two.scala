package interviews_new.wayfair

import scala.collection.mutable

/*
We are writing a tool to help users manage their calendars. Given an unordered list of times of day when someone is busy, write a function that tells us whether they're available during a specified period of time.

Each time is expressed as an integer using 24-hour notation, such as 1200 (12:00), 1530 (15:30), or 800 (8:00).

Sample input:

meetings = [
  [1230, 1300], // 12:30 PM to 1:00 PM
  [845, 900],   //  8:45 AM to 9:00 AM
  [1300, 1500]  //  1:00 PM to 3:00 PM
]

Expected output:

 1 - 10 = 9
 5 - 10 = 5


isAvailable(meetings, 915, 1215)   => true
isAvailable(meetings, 900, 1230)   => true
isAvailable(meetings, 1200, 1300)  => false
isAvailable(meetings, 850, 1240)   => false
isAvailable(meetings, 700, 1600)   => false
isAvailable(meetings, 800, 845)    => true
isAvailable(meetings, 1500, 1800)  => true
isAvailable(meetings, 845, 859)    => false
isAvailable(meetings, 846, 900)    => false
isAvailable(meetings, 846, 859)    => false
isAvailable(meetings, 845, 900)    => false
isAvailable(meetings, 2359, 2400)  => true
isAvailable(meetings, 930, 1600)   => false
isAvailable(meetings, 800, 850)    => false
isAvailable(meetings, 1400, 1600)  => false
isAvailable(meetings, 1300, 1501)  => false

n = number of meetings
r = minutes in range of meetings
*/

object Solution {

  val maxHeap = mutable.PriorityQueue.empty[String](Ordering.by((i:String) => i.length))
  val ss = mutable.Map[String, Int]()
  ss.get("").getOrElse(0)

  def isAvailable(meetings:Seq[(Int, Int)], start:Int, end:Int):Boolean = {

    val s =new Animal {}
    val ss = new Animal {}

    s == ss

    def isValidNum(): Boolean = {
      var validNum = true
      var i = 0
      while (i < meetings.length && validNum) {
        val start2 = meetings(i)._1
        val end2 = meetings(i)._2

        val total = Math.max(end2, end) - Math.min(start2, start)
        val sum = (end2-start2) + (end-start)

        if (sum > total) validNum = false
        i += 1
      }
      validNum
    }

    val result = isValidNum()
    println(s"${start}, ${end} => $result")
    result



  }

  def main(args: Array[String]): Unit = {

    val meetings: Seq[(Int, Int)] = Seq(
      (1230, 1300),
      (845, 900),
      (1300, 1500)
    )

    isAvailable(meetings, 915, 1215)
    isAvailable(meetings, 900, 1230)
    isAvailable(meetings, 1200, 1300)
    isAvailable(meetings, 850, 1240)
    isAvailable(meetings, 700, 1600)
    isAvailable(meetings, 800, 845)
    isAvailable(meetings, 1500, 1800)
    isAvailable(meetings, 845, 859)
    isAvailable(meetings, 846, 900)
    isAvailable(meetings, 846, 859)
    isAvailable(meetings, 845, 900)
    isAvailable(meetings, 2359, 2400)
    isAvailable(meetings, 930, 1600)
    isAvailable(meetings, 800, 850)
    isAvailable(meetings, 1400, 1600)
    isAvailable(meetings, 1300, 1501)

  }
}


