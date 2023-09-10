package lessons.designpatterns.structural

// Minimize memory footprint
// Classic Example: Word processing

sealed abstract class Color
case object Red extends Color
case object Blue extends Color
case object Green extends Color

case class Circle(color:Color)

// Companion objects with apply are used for factories as well
object Circle {
  // Flyweight container
  private val cache = collection.mutable.Map[Color, Circle]()

  def count:Int = cache.size
  def apply(color: Color):Circle = cache.getOrElseUpdate(color, new Circle(color))
}

class Graphic {
  private val circles = collection.mutable.ListBuffer[(Int, Int, Double, Circle)]()

  def addCircle(x:Int, y:Int, radius:Double, circle: Circle):Unit = {
    circles += ((x,y,radius,circle))
  }

  def draw():Unit = {
    println(s"Total number of circles in Graph: ${circles.size}")
    println(s"Total number of circles in RAM: ${Circle.count}")
    /*circles.foreach {
      case (x, y, radius, circle) => println(s"$x,$y,$radius,$circle")
    }*/
  }

}

object FlyWeightTest extends App {
  val graphic = new Graphic()
  graphic.addCircle(1,2,3,Circle(Blue))
  graphic.addCircle(1,2,3,Circle(Green))
  graphic.addCircle(1,2,3,Circle(Red))
  graphic.addCircle(1,2,3,Circle(Red))
  graphic.addCircle(1,2,3,Circle(Red))
  graphic.addCircle(1,2,3,Circle(Red))
  graphic.addCircle(1,2,3,Circle(Red))
  graphic.draw()
}
