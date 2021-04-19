package lessons.ik.mockinterviews

class One {

  /*
    After several months of rehearsal, the cows are just about
    ready to put on their annual dance performance; this year they are performing the
    famous bovine ballet "Cowpelia".
    The only aspect of the show that remains to be determined is the size of the stage.
    A stage of size K can support K cows dancing simultaneously.
    The N cows in the herd (1≤N≤10,000) are conveniently numbered 1…N in the order in
    which they must appear in the dance. Each cow i plans to dance for
    a specific duration of time d(i). Initially, cows 1…K appear on stage and start dancing.
    When the first of these cows completes her part, she leaves the stage and cow K+1
    immediately starts dancing, and so on, so there are always K cows dancing
    (until the end of the show, when we start to run out of cows).
    The show ends when the last cow completes her dancing part, at time T.

    Clearly, the larger the value of K, the smaller the value of T. Since the show cannot
    last too long, you are given as input an upper bound Tmax specifying the largest possible
    value of T. Subject to this constraint, please determine the smallest possible value of K.

    Tmax is an integer of value at most 1 million.
    Each d(i) value is an integer in the range 1…100,000.
    It is guaranteed that if K=N, the show will finish in time.
    Please find the smallest possible value of K such that the dance performance
    will take no more than Tmax units of time.
   */

  /*
        1. Choose a random K
            - Use binary search
                - If K is good then go left
                - If K is not good, got right (if right is the same as last K or N then exit)
        2. Decide if K is a solution
            - If we choose a min heap, we know that the bottom value is the largest number
            - Building the heap K sized will yield the largest number at the bottom (largest time)
            - If we pop the heap, add that number to the next value and push it back in the heap, we'll get a time
            - Once we do this for all the numbers in d, then pop the heap and the last value is T
            (This works because "In addition to Tmax-4 you have to add 4 more 'minues')
            (Because every time you pop, that's how much time it has passed. Adding the time that has passed with the
            next time coming, is a T. This will eventually push the true T to the bottom of the heap)

            EXAMPLE INPUT:

            Tmax = 8
            int[] d = {4, 7, 8, 6, 4, 1}
            EXAMPLE OUTPUT: 4
    */

}

object HelloWorld extends App {

//  println(findMinStageSize(Array(4,7,8,6,4), 8))
  def findMinStageSize(d:Array[Int], Tmax:Int):Int = {
    import scala.collection.mutable.{PriorityQueue}

    // Check if stage is valid
    def isValidStageSize(size:Int): Boolean = {
      val minHeap = PriorityQueue.empty[Int](Ordering.by((i:Int) => -i))

      for (i <- 0 until d.length) {
        if (minHeap.size < size)  {
          minHeap.enqueue(d(i))
        } else {
          minHeap.enqueue(minHeap.dequeue() + d(i))
        }
      }

      minHeap.max <= Tmax
    }

    var finish = false
    var lastSampleSize = -1
    var from = 0
    var to = d.length
    while (!finish) {
      val sampleSize = (from+to)/2

      if (sampleSize < 1 || sampleSize > Tmax) {
        finish = true
      } else {
        if (isValidStageSize(sampleSize)) {
          // Go left
          to = sampleSize-1
          lastSampleSize = sampleSize
        } else if (lastSampleSize == -1) {
          // Go right
          from = sampleSize+1
        } else {
          finish = true
        }
      }
    }

    lastSampleSize
  }

//  println(isValidIp("1.1.1.255"))
  def isValidIp(ip:String):Boolean = {
    // [1-255].[0-255].[0-255].[0-255]
    val tokens = ip.split("\\.")

    var result = true
    for (i <- 0 until tokens.length) {
      val num = tokens(i).toInt
      i match {
        case 0 => result = result && num > 0 && num < 256
        case 1 | 2 | 3 => result = result && num > -1 && num < 256
      }
    }

    result
  }

  println(isMatch("aaa", "..*"))
  def isMatch(text:String, pattern:String):Boolean = {
    /*
    Scenarios:
      1. c
      2. .
      3. c*
      4. .*
     */

    if (pattern.isEmpty()) return text.isEmpty()

    val first_match = !text.isEmpty() &&
      (pattern.charAt(0) == text.charAt(0) || pattern.charAt(0) == '.')

    if (pattern.length() >= 2 && pattern.charAt(1) == '*') {
       isMatch(text, pattern.substring(2)) ||
        (first_match && isMatch(text.substring(1), pattern))
    } else {
      first_match && isMatch(text.substring(1), pattern.substring(1))
    }
  }

}


/*
/* Regular Expression Matching
   ===========================

Implement regular expression matching with support for '.' and '*'.

'.' Matches any single character.
'*' Matches zero or more of the preceding element.
The matching should cover the entire input string (not partial).

The function prototype should be:

bool isMatch(string s, string p)
isMatch("aa","a") → false
isMatch("aa","aa") → true
isMatch("aaa","aa") → false
isMatch("aa", "a*") → true
isMatch("aa", ".*") → true
isMatch("ab", ".*") → true
isMatch("aab", "c*a*b") → true

Example 1:

Input："aa"，"a"
Output：false
Explanation：
unable to match
Example 2:

Input："aa"，"a*"
Output：true
Explanation：
'*' can repeat a
*/

boolean isMatch(String s, String p) {

}

/*
Write a function to check if a given string is valid IPv4 address if the dots were removed
[1-255].[0-255].[0-255].[0-255]

 */
1111 -> true (1.1.1.1)
9099 -> true (9.0.9.9)
255.022.255.255 --> false (255.022.255.255)
199199277255 --> 199.199.277.255 (false)
0111 -> 0.1.1.1 (false)
1000 -> 1.0.0.0 (true)


/*
Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center)

    1
   / \
  2   2
 / \ / \
3  4 4  3
return true
    1
   / \
  2   2
   \   \
   3    3

return false

              1
             / \
            2   2
           /     \
          3       3
         /         \
        7           7
*/

class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }

    public boolean isMirror(TreeNode root) {
        if (root == null) {
            return false; //throw exception
        }
        return helper(root, root);
    }

    public booelan helper(TreeNode node1, TreeNode node2) {
        if (node1 == null && node2 == null) {
            return true;
        }

        if (node1 == null  || node2 == null) {
            return false;
        }

        if (node1.val != node2.val) {
            return false;
        }

        return helper(node1.left, node2.right) && helper(node1.right, node2.left);

    }
}

 */