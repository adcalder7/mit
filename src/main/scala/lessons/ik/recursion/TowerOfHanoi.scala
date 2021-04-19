package lessons.ik.recursion


object TowerOfHanoi extends App {

  tower_of_hanoi(4).foreach(e => print(e.mkString(",") + " | "))
  def tower_of_hanoi(n: Int): Array[Array[Int]] = {
    import collection.mutable.Buffer

    val buf = Buffer[Array[Int]]()

    def play(i:Int = n, s:Int = 1, t:Int = 2, d:Int = 3):Unit = {
      if (i > 0) {
        play(i-1, s, d, t)
        if (s != 0) buf.append(Array(s, d))
        play(i-1, t, s, d)
      }
    }

    play()

    buf.toArray
  }

  // You have to work with n for recursive problems
  // and take a leap of faith
  // This is depth-first order
//  play(7)
  def play(n: Int):Unit = {
    import scala.collection.mutable.Stack

    val src:Stack[Int] = Stack[Int]()
    val dest:Stack[Int] = Stack[Int]()
    val aux:Stack[Int] = Stack[Int]()

    def permute(i:Int = 0,
                src:Stack[Int],
                dest:Stack[Int],
                aux:Stack[Int]):Unit = {
      if (i > 0) {
        // src to aux
        permute(i-1, src, aux, dest)

        // src to dest
        if (!src.isEmpty) dest.push(src.pop())

        // aux to dest
        permute(i-1, aux, dest, src)
      }
    }

    src.pushAll(n to 1 by -1)
    permute(n, src, dest, aux)

    println("src: " + src.mkString)
    println("dest: " + dest.mkString)
    println("aux: " + aux.mkString)
  }

}
