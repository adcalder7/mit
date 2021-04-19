package lessons.ik.recursion

object Driver extends App {
  println(Solution.fibonnaciBad(4))
  println(Solution.fibonnaciGood(4, 0, 1))
}

object Solution {

  def fibonnaciBad(n:Int):Int = {
    if (n == 0 || n == 1) {
      1
    } else {
      fibonnaciBad(n-1) + fibonnaciBad(n-2)
    }
  }

  def fibonnaciGood(n:Int, b1:Int, b2:Int):Int = {
    if (n == 0) {
      b1
    } else {
      fibonnaciGood(n-1, b2, b1 + b2)
    }
  }

  def permutations(node:String, acc:String = "", i:Int = 0): String = {
    if (node.length == 0) {
      acc(i) + "|"
    } else {
      permutations(node.substring(i+1), acc, i+1)
    }
  }

//  println(permutations("A"))

}
