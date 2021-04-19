package lessons.ik.mockinterviews

import scala.collection.mutable
import scala.collection.mutable.{Buffer, Map}

object ConsecutiveCharacter extends App {

  // TODO: Solve for k = 2
  // The answer below is k = 1

  println(getNonConsecutiveString("aaabbc"))

  class Ledger(s: String) {

    case class Item(c: Char, f: Int)

    val map = Map[Char, Int]()
    val bookKeeping = mutable.PriorityQueue.empty[Item](Ordering.by((i: Item) => i.f))

    s.foreach(c => {
      map.put(c, map.get(c).getOrElse(0) + 1)
    })

    map.foreach(kv => {
      bookKeeping.enqueue(Item(kv._1, kv._2))
    })

    def isEmpty(): Boolean = bookKeeping.isEmpty

    private def enqueue(c: Char, f: Int): Unit = {
      if (map(c) > 0) {
        bookKeeping.enqueue(Item(c, f))
      }
    }

    def getNextChar(c: Option[Char] = None): Option[Char] = {
      if (bookKeeping.isEmpty) {
        None
      } else if (bookKeeping.size == 1 || !c.isDefined) {
        val nextItem = bookKeeping.dequeue()
        map.put(nextItem.c, nextItem.f - 1)
        enqueue(nextItem.c, nextItem.f - 1)
        Some(nextItem.c)
      } else if (c.get == bookKeeping.head.c) {
        val top = bookKeeping.dequeue()
        val nextItem = bookKeeping.dequeue()
        bookKeeping.enqueue(top)
        map.put(nextItem.c, nextItem.f - 1)
        enqueue(nextItem.c, map(nextItem.c))
        Some(nextItem.c)
      } else {
        val nextItem = bookKeeping.dequeue()
        map.put(nextItem.c, nextItem.f - 1)
        enqueue(nextItem.c, map(nextItem.c))
        Some(nextItem.c)
      }
    }

  }

  def getNonConsecutiveString(s: String): String = {
    if (s.length < 2) return s

    val ledger = new Ledger(s)
    val output = Buffer[Char]()

    // Get next character is log n because of max heap
    var next: Option[Char] = None
    while (!ledger.isEmpty) {
      next = ledger.getNextChar(next)
      output.append(next.get)
    }

    // Check if output is actually non consecutive
    if (output(output.length - 1) == output(output.length - 2)) "Invalid Input"
    else output.mkString
  }

}
