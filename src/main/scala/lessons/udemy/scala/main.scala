package lessons.udemy.scala

object main extends App {

  var x, y ,z = 0

  def passByName(i: => Int): Int = {
    // Everytime i gets called. It will evaluate the passer
    println("first", i)
    println("second", i)
    i*i
  }
  println(passByName(util.Random.nextInt(10)))

  println(Array.tabulate(10)(i => i).mkString(", "))

  // Tree structure ln n always vs array/lists
  val vector = Vector(1,2,3)

  // Zipwithindex is just like zip(collection.indices)
  val array = Array(1,2,3,4,5,6)
  println(array.zip(array.indices).toMap.mkString(","))
  val array2 = Array(1,23)
  println(array.zipAll(array2, 1, -1).mkString(","))

  // Reduce
  println(array.reduce(_ + _))
  println(array.fold(0)(_ + _))

  // Folds
  val sb = StringBuilder.newBuilder
  println(array.foldLeft(sb:StringBuilder)((c:StringBuilder, v) => c.append(v)).toString)
  sb.clear()
  println(array.foldRight(sb:StringBuilder)((v, c:StringBuilder) => c.append(v)).toString)

  // Use yield to get values from for loops
  val likePython = for (i <- 0 until array.length) yield array(i)*2
  println(likePython.mkString(","))

  val fun = for {
    a <- 1 until 3
    if a % 2 == 0 // Filters
    b <- 11 until 15
    if b % 2 != 0 // Filters
  } yield (a, b)
  println(fun.mkString(","))

  val pf:PartialFunction[Any, String] = {
    case i:Int => s"You used an integer: $i"
    case b:String => s"You used a string: $b"
    case _ => "?"
  }
  println(pf(1), pf("hello"))

  // Amazing
  def myWhile(cond: => Boolean)(body: => Unit):Unit = {
    if (cond) {
      body
      myWhile(cond)(body)
    }
  }

  var increment = 0
  myWhile(increment<5) {
    println("hello")
    increment+=1
  }

  implicit def convert(s:String) = 1
  println("s" + 1)
}
