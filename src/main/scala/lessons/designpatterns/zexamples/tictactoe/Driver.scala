package lessons.designpatterns.zexamples.tictactoe

import scala.collection.mutable

trait Observable[T] {
  def myNotify:Boolean
}

trait Observer[T] {
  def myNotify:Boolean
}

case class Location(name:String)

class Board extends Observable [Board] {
  private val locations = mutable.Set[Location]()

  override def myNotify: Boolean = ???
}

object Driver extends App {
  println(0x0)
}
