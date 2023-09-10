package lessons.designpatterns.structural

// When you want to add functionality to a class without extending it or modifying it

// Decorators do not use "depends on" because the decorated class should have 'IS A' relationship
// and the overrides should be clear

class DoNotTouch {
  def getCode:String = "12345"
}

trait CapitalizedDoNotTouch extends DoNotTouch {
  override def getCode: String = super.getCode.toUpperCase()
}

trait ReversedDoNotTouch extends DoNotTouch {
  override def getCode: String = super.getCode.reverse
}

object Decorator extends App {
  val capitalizedCode:DoNotTouch = new DoNotTouch with CapitalizedDoNotTouch
  val reversedCode:DoNotTouch = new DoNotTouch with ReversedDoNotTouch
}
