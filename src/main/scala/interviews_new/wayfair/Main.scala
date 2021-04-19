package interviews_new.wayfair

import scala.collection.mutable

trait Quadruple {
  def cleanBackLegs
}

abstract class Animal {
  val name:String = ""
}

case class Dog(override val name:String) extends Animal with Quadruple {
  def this() = this("stray")

  override def cleanBackLegs: Unit = {
    println(s"Cleaning all legs of dog $name")
  }
}

object Dog {
  def newDog(name:String) = new Dog(name)

  def stray() = new Dog()
}

object Main extends App {

  def findNodesWithZeroAndOneParents(v:List[(Int, Int)]):(Set[Int], Set[Int]) = {
    val parents = mutable.Set[Int]()
    val children = mutable.Set[Int]()
    val childrenParentMap = mutable.Map[Int, mutable.ListBuffer[Int]]()

    // Build list of parents and children
    for (i <- 0 until v.length) {
      parents += v(i)._1
      children += v(i)._2
      childrenParentMap.put(v(i)._2,
        childrenParentMap.getOrElse(v(i)._2, new mutable.ListBuffer[Int]) += v(i)._1)
    }

    val pr = parents.filter(e => !children.contains(e)).toSet
    val cr = childrenParentMap.filter(p => p._2.size < 2).keys.toSet

    (pr, cr)
  }

  val (parents, children) = findNodesWithZeroAndOneParents(List((1,2),(1,2),(1,2),(1,2)))
  println(parents.mkString("[",",","]"))
  println(children.mkString("[",",","]"))

  val pepe = Dog.newDog("Pepe")
  pepe.cleanBackLegs
  val unknown = Dog.stray()
  unknown.cleanBackLegs
}
