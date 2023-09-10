package lessons.designpatterns.creational

sealed trait Animal {
  def speak: Unit
}

sealed trait Dog extends Animal
private class LittleDog extends Dog  { def speak = println("arf")   }
private class GenericDog extends Dog { def speak = println("woof")  }
private class BigDog extends Dog     { def speak = println("WOOF!") }

sealed trait Cat extends Animal
private class GenericCat extends Cat { def speak = println("meow")    }
private class GrumpyCat extends Cat  { def speak = println(":(")      }
private class SmellyCat extends Cat  { def speak = println("..oOo..") }

// Usually a factory has a list of parameters to boil down to a specific instance of a class
trait AnimalFactory {
  def getAnimal(criteria: String): Animal
}

class DogFactory extends AnimalFactory {
  def getAnimal(criteria: String): Animal = criteria match {
    case "small" | "little" => new LittleDog
    case "big" | "large" => new BigDog
    case _ => new GenericDog
  }
}

class CatFactory extends AnimalFactory {
  def getAnimal(criteria: String) = criteria match {
    case "smells" | "smelly" => new SmellyCat
    case "grumpy" => new GrumpyCat
    case _ => new GenericCat
  }
}

object SimpleFactory {
  // The Abstract Factory would use a base factory like AnimalFactory
  // and pass DogFactory/CatFactory as parameters
  // Simple Factory instead forces users to use DogFactory/CatFactory
  val dog = new DogFactory().getAnimal("small")
  val cat = new CatFactory().getAnimal("small")
}
