package lessons.designpatterns.creational

// If a class will have to have many constructors, use a builder

case class PersonBuilder(firstName:String = "", lastName:String = "", age:Int = 0) {
  var build = new Person(this)
}

class Person(builder: PersonBuilder) {
  val firstName = builder.firstName
  val lastName = builder.lastName
  val age = builder.age
}

object Builder extends App {
  val person:Person = PersonBuilder("x", "y", 3).build

  var j = 0
  var total = 0
  for (i <- 0 until 5) {
    total += i - j + 1
    println(total)
  }
}
