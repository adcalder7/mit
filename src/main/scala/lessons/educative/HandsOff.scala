package lessons.educative

object MultipleString extends App {

  println(three("xxa","xaabb"))
  def three(a:String, b:String):Int = {

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (i >= a.length || j >= b.length) return 0

      if (a.charAt(i) == b.charAt(j)) {
        return 1 + permute(i+1, j+1)
      }

      Math.max(permute(i+1,j), permute(i,j+1))
    }

    val lcs = permute()
    (a.length - lcs) + (b.length - lcs)
  }

//  println(two("abc", "acc"))
  def two(a:String, b:String):Int = {

    def permute(i:Int = 0, j:Int = 0):Int = {
      if (i >= a.length || j >= b.length) return 0

      if (a.charAt(i) == b.charAt(j)) {
        return 1 + permute(i+1, j+1)
      }

      Math.max(permute(i+1, j), permute(i, j+1))
    }

    permute()
  }

//  println(one("abc", "axbxcx"))
  def one(a:String, b:String):Int = {

    def permute(i:Int = 0, j:Int = 0, c:Int = 0):Int = {

      if (i >= a.length || j >= b.length) return c

      val xx = if (a.charAt(i) == b.charAt(j)) {
        permute(i+1, j+1, c+1)
      } else 0

      val yy = Math.max(permute(i+1, j), permute(i, j+1))
      Math.max(xx, yy)
    }

    permute()
  }

}

object SingleString extends App {

//  println(five("aabcdaa"))
  def five(s:String):Int = {

    def permute(i:Int = 0, j:Int = s.length-1):Int = {
      if (i > j || isPalindrome(i, j)) return 0

      // This is the level count
      var levelCut = j - i
      var k = i

      while (k < j) {
        println(s.substring(k, j))
        // if we do better then let's compare
        val cut = if (isPalindrome(k, j)) 1 + permute(k+1, j) else levelCut
        levelCut = Math.min(levelCut, cut)
        k+=1
      }

      levelCut
    }

    def isPalindrome(b:Int, e:Int):Boolean = {
      var x = b
      var y = e
      while (x <= y && s.charAt(x) == s.charAt(y)) {
        x += 1
        y -= 1
      }
      x > y
    }

    permute()
  }

//  println(three("aaaa"))
  def three(s:String):Int = {

    def permute(b:Int = 0, e:Int = s.length-1):Int = {

      if (b == e) return 1
      else if (b > e) return 0

      var count = 0

      // You have to investigate it before you dive in
      if (isPal(b, e)) count = 1

      count += permute(b+1,e)
      count += permute(b,e-1)
      count -= permute(b+1,e-1)

      count
    }

    def isPal(b:Int = 0, e:Int = 0):Boolean = {
      var x = b
      var y = e
      while (x <= y && s.charAt(x) == s.charAt(e)) {
        x += 1
        y -= 1
      }
      x > y
    }

    permute()
  }

//  println(two("abba"))
  def two(s:String):Int = {

    def permute(i:Int = 0, j:Int = s.length-1):Int = {
      if (i == j) return 1
      else if (i > j) return 0

      if (s.charAt(i) == s.charAt(j)) {
        // Check if the inside is a palindrome as well
        val inside = j - i - 1
        if (inside == permute(i+1, j-1)) {
          return 2 + inside
        }
      }

      Math.max(permute(i+1, j), permute(i, j-1))
    }

    permute()
  }

//  println(one("pqr"))
  def one(s:String):Int = {

    // Is palindrome
    def permute(i:Int = 0, j:Int = s.length-1):Int = {
      if (i >= j) return 1

      if (s.charAt(i) == s.charAt(j)) {
        return 2 + permute(i+1, j-1)
      }

      Math.max(permute(i+1, j), permute(i, j-1))
    }

    permute()
  }

}

object fibonacci extends App {

//  println(seven(Array(2, 10, 14, 8, 1)))
  def seven(a:Array[Int]):Int = {

    def permute(i:Int = 0, p:Int = 0):Int = {
      if (i >= a.length) return p

      val c1 = permute(i+1, p)
      val c2 = permute(i+2, a(i) + p)

      Math.max(c1, c2)
    }

    permute()
  }

//  println(six(Array(2,3,4,5), 4))
  def six(a:Array[Int], n:Int = 0):Int = {

    def permute(i:Int = 0, cc:Int = 0):Int = {
      if (i == n) return cc
      else if (i >= n) return Integer.MAX_VALUE

      val s1 = if (i+1 <= n) permute(i+1, cc + a(i)) else Integer.MAX_VALUE
      val s2 = if (i+2 <= n) permute(i+2, cc + a(i)) else Integer.MAX_VALUE
      val s3 = if (i+3 <= n) permute(i+3, cc + a(i)) else Integer.MAX_VALUE

      Math.min(s1, Math.min(s2, s3))
    }

    val x = permute()
    if (x == Integer.MAX_VALUE) 0 else x
  }

//  println(four(Array(1,1,3,6,9,3,0,1,3)))
  def four(a:Array[Int]):Int = {

    def permute(i:Int = 0, s:Int = 0):Int = {

      // The last number doesn't count
      if (i == a.length-1) return s
      else if (i >= a.length) return Integer.MAX_VALUE

      var levelCount = Integer.MAX_VALUE
      var j = i
      val to = i + a(i)
      while (j < a.length && j < to) {
        levelCount = Math.min(levelCount, permute(j+1, s+1))
        j+=1
      }

      levelCount
    }

    val result = permute()
    if (result == Integer.MAX_VALUE) 0 else result
  }

//  println(three(5))
  def three(n:Int):Int = {

    def permute(i:Int = 0):Int  = {
      if (i >= n) return 1

      val c1 = if (i+1 <= n) permute(i+1) else 0
      val c2 = if (i+3 <= n) permute(i+3) else 0
      val c3 = if (i+4 <= n) permute(i+4) else 0

      c1 + c2 + c3
    }

    permute()
  }

//  println(two(4))
  def two(n:Int):Int = {

    def permute(i:Int = 0):Int  = {
      if (i >= n) return 1

      val c1 = if (i+1 <= n) permute(i+1) else 0
      val c2 = if (i+2 <= n) permute(i+2) else 0
      val c3 = if (i+3 <= n) permute(i+3) else 0

      c1 + c2 + c3
    }

    permute()
  }

//  println(one(7))
  def one(n:Int):Int = {

    def permute(i:Int = n):Int = {
      if (i <= 0) return 0
      if (i == 1 || i == 2) return 1

      permute(i-1) + permute(i-2)
    }

    permute()
  }

}

object UnboundedKnapsack extends App {

//  println(five(Array(2,3,5), 5))
  def five(a:Array[Int], n:Int):Int = {

    def permute(i:Int = 0, nn:Int = 0):Int = {
      if (nn > n || i >= a.length) return 0

      val c1 = if (nn+a(i) <= n) 1 + permute(i, nn+a(i)) else 0
      val c2 = permute(i+1, nn)

      Math.max(c1, c2)
    }

    permute()
  }

//  println(four(Array(1,2,3), 5))
  def four(a:Array[Int], c:Int):Int = {

    def permute(i:Int = 0, cc:Int = 0):Int = {
      if (cc == c) return 0
      else if (cc > c || i >= a.length) return Integer.MAX_VALUE

      val c1 = if (cc+a(i) <= c) {
        val x = permute(i, cc+a(i))
        if (x != Integer.MAX_VALUE) {
          1 + permute(i, cc+a(i))
        } else Integer.MAX_VALUE
      } else Integer.MAX_VALUE
      val c2 = permute(i+1, cc)

      Math.min(c1, c2)
    }

    permute()
  }

//  println(three(Array(1,2,3), 5))
  def three(a:Array[Int], c:Int):Int = {

    def permute(i:Int = 0, cc:Int = 0):Int = {
      if (cc == c) return 1
      else if (cc > c || i >= a.length) return 0

      val c1 = if (cc+a(i) <= c) permute(i, cc+a(i)) else 0
      val c2 = permute(i+1, cc)

      c1 + c2
    }

    permute()
  }

//  println(two(Array(1,2,3,4,5), Array(2,6,7,10,13), 5))
  def two(a:Array[Int], p:Array[Int], l:Int):Int = {

    def permute(i:Int = 0, sl:Int = 0):Int = {
      if (sl > l || i >= a.length) return 0

      val c1 = if (sl+a(i) <= l) p(i) + permute(i, sl+a(i)) else 0
      val c2 = permute(i+1, sl)

      Math.max(c1, c2)
    }

    permute()
  }

//  println(one(Array(15, 50, 60, 90), Array(1, 3, 4, 5), 6))
  def one(p:Array[Int], w:Array[Int], t:Int):Int = {

    def permute(i:Int = 0, st:Int = t):Int = {
      if (st <= 0 || i >= p.length) return 0

      val c1 = if (st-w(i) >= 0) p(i) + permute(i, st-w(i)) else 0
      val c2 = permute(i+1, st)

      Math.max(c1, c2)
    }

    permute()
  }

}

object BoundedKnapsack extends App {

//  println(six(Array(1,2,7,1), 9))
  def six(a:Array[Int], t:Int):Int = {

    def permute(i:Int = 0, st:Int = 0):Int = {

      if (i >= a.length) {
        if (st == t) return 1
        return 0
      }

      val c1 = permute(i+1, st + a(i))
      val c2 = permute(i+1, st - a(i))

      c1 + c2
    }

    permute()
  }

//  println(five(Array(1,2,7,1,5), 9))
  def five(a:Array[Int], t:Int):Int = {

    def permute(i:Int = 0, st:Int = 0):Int = {
      if (st == t) return 1
      else if (i >= a.length) return 0

      val c1 = permute(i+1, st + a(i))
      val c2 = permute(i+1, st)

      c1 + c2
    }

    permute()
  }

//  println(four(Array(1,3,100,4)))
  def four(a:Array[Int]):Int = {

    def permute(i:Int = 0, left:Int = 0, right:Int = a.sum):Int = {
      if (i >= a.length) return Math.abs(left - right)

      val c1 = permute(i+1, left + a(i), right - a(i))
      val c2 = permute(i+1, left, right)

      Math.min(c1, c2)
    }

    permute()
  }

//  println(three(Array(1,3,4,8), 6))
  def three(a:Array[Int], t:Int):Boolean = {

    def permute(i:Int = 0, s:Int = 0):Boolean = {
      if (s == t) return true
      else if (i >= a.length) return false

      val c1 = permute(i+1, s+a(i))
      val c2 = permute(i+1, s)

      c1 || c2
    }

    permute()
  }

//  println(two(Array(2,3,4,6)))
  def two(a:Array[Int]):Boolean = {

    // Target number

    val sum = a.sum
    if (sum % 2 != 0) return false
    val target = sum/2

    def permute(i:Int = 0, s:Int = 0):Boolean = {

      if (s == target) return true
      if (i >= a.length) return false

      val c1 = permute(i+1, s + a(i))
      val c2 = permute(i+1, s)

      c1 || c2
    }

    permute()
  }

//  println(one(Array(1,6,10,16), Array(1,2,3,5), 7))
  def one(p:Array[Int], w:Array[Int], limit:Int):Int = {

    def permute(i:Int = 0, cw:Int = limit):Int = {
      if (cw <= 0 || i >= p.length) return 0

      val c1 = if (cw-w(i) >= 0) p(i) + permute(i+1, cw - w(i)) else 0
      val c2 = permute(i+1, cw)

      Math.max(c1, c2)
    }

    permute()
  }

}
