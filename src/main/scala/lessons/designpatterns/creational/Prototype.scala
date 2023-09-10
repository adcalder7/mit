package lessons.designpatterns.creational

// Copy and object vs creating a brand new instance
// Side effects may occur

case class ExpensiveConstructor(obj1:String, obj2:String)

object Prototype {
  val ex1 = ExpensiveConstructor("one", "two")
  val ex2 = ex1.copy()
}
