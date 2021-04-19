package lessons.udemy.scala.oop

case class OOP(i:Int, j:Int) {
  // Constructors
  def this() = this(0,0)
  def this(i:Int) = this(i, 0)
  def this(i:Double) = this(0,0)

  def +(other:OOP) = {
    OOP(this.i + other.i, this.j + other.j)
  }
}

object OOP {

  def apply():OOP = {
    OOP(3,3)
  }

  def sum:Int = {
    0
  }

}

object Run extends App {
  val oop1 = OOP()
  val oop2 = OOP(1,2)

  println(oop1 + oop2)
  println(OOP.sum)
}
