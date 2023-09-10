package lessons.designpatterns.behavioral

/*
The strategy pattern is one of the simpler patterns to comprehend.
It allows grouping related algorithms under an abstraction, which the
client codes against. The abstraction allows switching out one algorithm or policy
for another without modifying the client.

The strategy pattern is formally defined as encapsulating algorithms belonging
to the same family and making them interchangeable. The consumers of the common
interface that the algorithms implement allow switching out one algorithm for another seamlessly.
 */

object Parser {
  // Factory Method
  def apply(name:String): () => List[String] = {
    name match {
      case f if f.endsWith(".json") => CSVParser
      case f if f.endsWith(".csv") => JSONParser
      case _ => null
    }
  }

  // Concrete implementations
  def CSVParser():List[String] = List("csv1", "csv2", "csv3")
  def JSONParser():List[String] = List("json1", "json2", "json3")
}

// Abstract factory but it takes a function instead of a class
class Client(strategyLambda:() => List[String]) {
  def compute:Unit = strategyLambda().foreach(println)
}

// This is the application that decides what algorithm to use
object Strategy extends App {
  new Client(Parser("sample.csv")).compute
  new Client(Parser("sample.json")).compute
}
