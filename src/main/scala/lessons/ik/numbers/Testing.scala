package lessons.ik.numbers

object Testing extends App {

//  def minimalHeaviestSetA(arr: Array[Int]): Array[Int] = {
//    // Write your code here
//
//    return Array()
//
//    "angel".drop(0)
//  }

  def lengthOfLastWord(s: String): Int = {
    // If last word doesn't exist return 0
    // word = no spaces

    // word1 word2

    // TC = O(N)
    // SP = O(T) where T is the amount of words

    // "a b      c d" = array(a,b,c,d)

    s.trim.split(" ").last.size
  }

  println(lengthOfLastWord("jahsdlkfj lkajsdflkjha dlkjf aaslkjdhflkajshdflk"))

}
