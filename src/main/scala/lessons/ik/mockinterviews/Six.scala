package lessons.ik.mockinterviews

/**
Write a f that calc the number of ways to sum to a total of x, by using the values contained in a set s

x = 7
s = 2 3 7

7 = 7
7 = 2 2 3
7 = 2 3 2
7 = 3 2 2

There are 4 ways to sum 7 using the values in 2 3 7

Note
x and s are positive integers
Can use elements in s multiple times, or none
Diff orderings => diff ways


x = 100, s = 1

x
|
x-1
|
x-2
|
0 = x-x

     x
/.........\
s1 s2 ... sn


              7
          /   |    \
      (2)5  4(3)  (7)0
        /|\
    (2)3   2(3)
      /|\
  (2)1   0(3)
    /
   x

   O(c^n)
   O(n)


         7
    /    |    \
   2     3
  /|\   /|\
   3   2
   |   |
   2   2
  /|\  *
 2 3 7
 0 - -

 f(2, [2,3,7]) => 1

 2 => 1



Map<Total, Count>
Map(2, 1)
 */

object Solution extends App {

  println(calculate(7, Array(2,3,7)))

  def calculate(x:Int, s:Array[Int]):Int = {
    import collection.mutable.Map

    val cache = Map[Int, Int]()

    def permute(sum:Int = x): Int = {
      if (cache.contains(sum)) return cache(sum)

      var levelCount = 0

      if (sum == 0) return 1
      else if (sum > 0) {
        for (i <- 0 until s.size) levelCount += permute(sum - s(i))
      }

      cache.put(sum, levelCount)
      levelCount
    }

    permute()
  }

}
