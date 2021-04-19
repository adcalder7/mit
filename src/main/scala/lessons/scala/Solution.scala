package lessons.scala

object Solution extends App {

  class GetterSetter {
    // If you don’t want any getter or setter, declare the field as private[this]
    private var something = 0

    def this(i:Int) = {
      this()
      this.something = i
    }

    // This is how you override a getter
    def ss:Int = something

    // This is how you override a setter
    def ss_=(other:Int): Int = {
      something = other
      other
    }
  }

  def practice():Unit = {
    val gs = new GetterSetter()
    gs.ss = 399

    for (i <- 0 until 2; j <- 0 until 5 if j % 2 == 0) {
      println(i, j)
    }

    println((for (i <- 0 until 10) yield s"a_${i}").mkString(","))

    println(List(1,2,2,2,2,4,5,6).distinct)

    val arr = Array(1,2,3,4,5,6)
    for (e <- arr if e != 4) {
      print(e + "-")
    }

    // Zip bundles things together
    val symbols = Array("<", "-", ">")
    val counts = Array(2, 10, 2)
    val pairs = symbols.zip(counts)


  }

  practice()

}
