package lessons.udemy.scala.oop

abstract class MyList[+A] {
  def head:A
  def tail:MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B):MyList[B]
  def printElements: String

  override def toString(): String = if (tail.isEmpty) "end" else head + " " + tail.printElements
}

object Empty extends MyList[Nothing] {
  def head:Nothing = ???
  def tail:MyList[Nothing] = ???
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B):MyList[B] = new InheritedList[B](element, Empty)
  def printElements: String = ???

  override def toString(): String = s"FROM EMPTY ${}"
}

class InheritedList[+A](headParam: A, tailParam: MyList[A] = Empty) extends MyList[A] {
  override def head: A = headParam
  override def tail: MyList[A] = tailParam
  override def isEmpty: Boolean = false
  override def add[B >: A](element: B) = new InheritedList[B](element, this)
  override def printElements: String =
    if (tailParam.isEmpty) "what" else headParam + " " + tailParam.printElements

  override def toString(): String = if (tail.isEmpty) "endx" else head + " " + tail.printElements
}

object InheritedList {
  def apply[B](items:B*):InheritedList[B] = {
    if (items != null && items.length > 0) {
      var list = new InheritedList[B](items(0))
      for (i <- 0 until items.length) {
        list = list.add(items(i))
      }
      list
    } else null
  }
}

object Run2 extends App {
  val list = InheritedList((for (i <- 0 until 10) yield i):_ *)
  val list2 = Empty

  println(list)
  println(list2)
}
