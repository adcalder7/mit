package lessons.datastructures.binarytree

import scala.collection.mutable

object Use extends App {
  val bt = new BinaryTree()
  val rnd = new scala.util.Random()

  bt.insert(44)
  bt.insert(17)
  bt.insert(88)
  bt.insert(8)
  bt.insert(28)
  bt.insert(65)
  bt.insert(97)
  bt.insert(29)
  bt.insert(54)
  bt.insert(82)
  bt.insert(93)
  bt.insert(76)
  bt.insert(68)
  bt.insert(80)

  bt.printAll()

  println(s"Min: ${bt.min()}")
  println(s"Max: ${bt.max()}")
  val (p, c) = bt.find(80)
  println(s"Predecessor of ${c} is ${bt.predecessor(80)}")
  println(s"Successor of ${c} is ${bt.successor(80)}")
  println(s"Deleting -1 ${bt.delete(-1)}")
  println(s"Delete case 1 68 ${bt.delete(68)}")
  bt.printAll()
  println(s"Delete case 2 28 ${bt.delete(28)}")
  bt.printAll()
  println(s"Delete case 3 44 ${bt.delete(44)}")
  bt.printAll()
  println(s"BFS")
  bt.printBFS()
}

class BinaryTree {
  private var root:Node = null

  class Node(var value:Int) {
    var left:Node = null
    var right:Node = null

    override def toString: String = value.toString
  }

  def buildTreeFromPreOrder(preOrdered:Array[Int]): Unit = {
    root = null
    val inOrder = preOrdered.sorted

    def permute(): Unit = {
      
    }

    permute()
  }

  def insert(v:Int):Unit = {
    var parent = root
    var next = root
    while (next != null) {
      if (next.value.compareTo(v) > 0) {
        parent = next
        next = next.left
      } else if (next.value.compareTo(v) <= 0) {
        parent = next
        next = next.right
      }
    }

    if (parent == null) root = new Node(v)
    else if (parent.value.compareTo(v) > 0) parent.left = new Node(v)
    else parent.right = new Node(v)
  }

  def delete(v:Int):Boolean = {
    if (root == null) return false

    // Search v
    var leftOfParent = false
    var parent:Node = null
    var node:Node = root
    var cont = true
    while (cont) {
      if (node.value.compareTo(v) > 0 && node.left != null) {
        leftOfParent = true
        parent = node
        node = node.left
      } else if (node.value.compareTo(v) < 0 && node.right != null) {
        leftOfParent = false
        parent = node
        node = node.right
      } else cont = false
    }

    if (node != null && node.value == v) {
      // Case 1: Leaf or root (Easy)
      if (node.left == null && node.right == null) {
        if (parent == null) {
          // Delete root
          root = null
        } else {
          // Delete leaf node
          if (parent.value.compareTo(v) > 0) {
            parent.left = null
          } else parent.right = null
        }
        true
      }
      // Case 2: Node has 1 child only (Easy)
      else if (node.left != null && node.right == null) {
        if (leftOfParent) parent.left = node.left
        else parent.right = node.left
        true
      } else if (node.left == null && node.right != null) {
        if (leftOfParent) parent.left = node.right
        else parent.right = node.right
        true
      }
      // Case 3: Node has 2 children
      else {
        var minOfRight:Node = node.right
        while (minOfRight.left != null) minOfRight = minOfRight.left

        // - Delete successor (using step 1 or 2)
        // - Can be optimized if not recursive because recursive starts at root
        delete(minOfRight.value)

        // - Swap v with successor
        minOfRight.left = node.left
        minOfRight.right = node.right
        if (parent == null) {
          root = minOfRight
        } else {
          parent.right = minOfRight
        }
        true
      }
    } else false
  }

  def find(v:Int):(Node, Node) = {
    var parent:Node = null
    var child = root
    var cont = true
    while (cont) {
      if (child.value.compareTo(v) > 0 && child.left != null) {
        parent = child
        child = child.left
      } else if (child.value.compareTo(v) < 0 && child.right != null) {
        parent = child
        child = child.right
      } else {
        cont = false
      }
    }
    (parent, child)
  }
  
  def predecessor(v:Int, start:Node = root):Node = {
    if (start == null) return null

    var predecessor:Node = null
    var node = start
    var cont = true
    while (cont) {
      if (node.value.compareTo(v) > 0 && node.left != null) {
        node = node.left
      } else if (node.value.compareTo(v) < 0 && node.right != null) {
        predecessor = node
        node = node.right
      } else cont = false
    }

    if (node.left != null) max(node.left)
    else predecessor
  }

  def successor(v:Int, start:Node = root):Node = {
    if (start == null) return null

    var successor:Node = null
    var node = start
    var cont = true
    while (cont) {
      if (node.value.compareTo(v) > 0 && node.left != null) {
        successor = node
        node = node.left
      } else if (node.value.compareTo(v) < 0 && node.right != null) {
        node = node.right
      } else cont = false
    }

    if (node.right != null) min(node.right)
    else successor
  }

  def min(start:Node = root): Node = {
    if (start == null) return null
    var node = start
    while (node.left != null) node = node.left
    node
  }

  def max(start:Node = root): Node = {
    if (start == null) return null
    var node = start
    while (node.right != null) node = node.right
    node
  }

  def printAll():Unit = {
    print("PreOrder: ")
    printPreOrder()
    print("InOrder: ")
    printInOrder()
    print("PosOrder: ")
    printPosOrder()
  }

  def printPreOrder():Unit = {
    def permute(node:Node):Unit = {
      if (node != null) {
        print(node.value + " ")
        permute(node.left)
        permute(node.right)
      }
    }

    permute(root)
    println()
  }

  def printInOrder():Unit = {
    def permute(node:Node):Unit = {
      if (node != null) {
        permute(node.left)
        print(node.value + " ")
        permute(node.right)
      }
    }

    permute(root)
    println()
  }

  def printPosOrder():Unit = {
    def permute(node:Node):Unit = {
      if (node != null) {
        permute(node.left)
        permute(node.right)
        print(node.value + " ")
      }
    }

    permute(root)
    println()
  }

  def printBFS():Unit = {
    if (root == null) return

    val queue = mutable.Queue[Node]()
    queue.enqueue(root)

    while (!queue.isEmpty) {
      val node = queue.dequeue()
      print(node.value + " ")
      if (node.left != null) queue.enqueue(node.left)
      if (node.right != null) queue.enqueue(node.right)
    }
    println()
  }
}
