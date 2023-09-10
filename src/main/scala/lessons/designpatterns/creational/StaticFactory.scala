package lessons.designpatterns.creational

package appdesign.reational.staticfactory {

  trait Animal
  class Cat extends Animal
  class Dog extends Animal
  class Bird extends Animal

  object StaticFactory extends App {
    def factory(name: String): Animal = {
      name match {
        case "bird" => new Bird()
        case "Cat" => new Cat()
        case "Dog" => new Dog()
        case _ => new Animal {}
      }
    }
  }

}
