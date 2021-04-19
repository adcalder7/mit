package lessons.ik.trees

class Node(val data_ : Int){
  var key: Int = data_
  var left: Node = null
  var right: Node = null
}
class TreeNode(val data_ : Int){
  var data: Int = data_
  var left_ptr: TreeNode = null
  var right_ptr: TreeNode = null
}

object Problems extends App {

  val tree = new TreeNode(200)
  tree.left_ptr = new TreeNode(100)
  tree.right_ptr = new TreeNode(300)
//  tree.left_ptr.left_ptr = new TreeNode(4)
//  tree.left_ptr.right_ptr = new TreeNode(5)
//  tree.left_ptr.left_ptr.left_ptr = new TreeNode(6)
//  tree.left_ptr.left_ptr.right_ptr = new TreeNode(7)

  val tree1 = new Node(3)
  tree1.right = new Node(4)
  tree1.right.right = new Node(5)
  val tree2 = new Node(7)
  tree2.left = new Node(6)
  tree2.left.left = new Node(5)

  println(isSymetric(tree))
  def isSymetric(root: TreeNode):Boolean = {
    if (root == null) return true

    def permute(left:TreeNode, right:TreeNode):Boolean = {
      if (left == null && right == null) true
      else if (left == null && right != null || left != null && right == null) false
      else {
        left.data == right.data &&
          permute(left.left_ptr, right.right_ptr) &&
          permute(right.left_ptr, left.right_ptr)
      }
    }

    permute(root.left_ptr, root.right_ptr)
  }

  def traverse1(node:TreeNode):Unit = {
    if (node != null) {
      traverse1(node.left_ptr)
      print(node.data + ",")
      traverse1(node.right_ptr)
    }
  }
//  traverse1(flipUpsideDown(tree))
//  println()
  def flipUpsideDown(root: TreeNode): TreeNode = {
    var head = root
    var oldHead:TreeNode = null
    var oldBottom:TreeNode = null

    while (head != null) {
      val newHead = head.left_ptr
      head.left_ptr = oldBottom
      val bottom = head.right_ptr
      head.right_ptr = oldHead
      oldBottom = bottom
      oldHead = head
      head = newHead
    }

    oldHead
  }

  def traverse(node:Node):Unit = {
    if (node != null) {
      traverse(node.left)
      print(node.key + ",")
      traverse(node.right)
    }
  }
//  traverse(mergeTwoBSTs(tree1, tree2))
//  println()
  def mergeTwoBSTs(root1: Node, root2: Node): Node = {
    val buffer = collection.mutable.Buffer[Int]()
    val bufferA = collection.mutable.Buffer[Int]()
    val bufferB = collection.mutable.Buffer[Int]()

    def inOrderTraversal(node:Node, col:collection.mutable.Buffer[Int]):Unit = {
      if (node == null) return
      inOrderTraversal(node.left, col)
      col.append(node.key)
      inOrderTraversal(node.right, col)
    }

    def merge(): Unit = {
      var p1 = 0
      var p2 = 0
      while (p1 < bufferA.length && p2 < bufferB.length) {
        if (bufferA(p1) <= bufferB(p2)) {
          buffer.append(bufferA(p1))
          p1+=1
        } else {
          buffer.append(bufferB(p2))
          p2+=1
        }
      }

      while (p1 < bufferA.length) {
        buffer.append(bufferA(p1))
        p1+=1
      }

      while (p2 < bufferB.length) {
        buffer.append(bufferB(p2))
        p2 += 1
      }
    }

    def buildTree(start: Int, end: Int): Node = {
      if (start <= end) {
        val mid = (start + end) / 2
        val root = new Node(buffer(mid))
        root.left = buildTree(start, mid - 1)
        root.right = buildTree(mid + 1, end)
        root
      } else null
    }

    inOrderTraversal(root1, bufferA)
    inOrderTraversal(root2, bufferB)
    merge()
    buildTree(0, buffer.length)
  }

//  println(lca(tree, new TreeNode(1), new TreeNode(3)))
  def lca(root: TreeNode, a: TreeNode, b: TreeNode):Integer = {
    if (root == null) null
    else if (root.data == a.data || root.data == b.data) root.data
    else {
      val left = lca(root.left_ptr, a, b)
      val right = lca(root.right_ptr, a, b)

      if (left != null && right != null) root.data
      else if (left == null && right == null) null
      else if (left != null) left
      else right
    }
  }

  def getHeight(tree: TreeNode):Int = {
    if (tree == null) 0
    else Math.max(getHeight(tree.left_ptr), getHeight(tree.right_ptr)) + 1
  }

//  printBTtoLL()
  def printBTtoLL() {
    var it = BTtoLL(tree)
    while (it != null) {
      print(it.data)
      it = it.right_ptr
    }
    println()
  }
  def BTtoLL(root: TreeNode): TreeNode = {
    var listRoot:TreeNode = null
    var listPrevNode:TreeNode = null

    def permute(node:TreeNode):Unit = {
      if (node != null) {
        permute(node.left_ptr)

        if (listRoot == null) {
          listRoot = new TreeNode(node.data)
          listPrevNode = listRoot
        } else {
          val at = new TreeNode(node.data)
          at.left_ptr = listPrevNode
          listPrevNode.right_ptr = at
          listPrevNode = at
        }

        permute(node.right_ptr)
      }
    }

    permute(root)
    listRoot
  }

//  allPathsOfABinaryTree(tree).foreach(e => {println(e.mkString(","))})
  def allPathsOfABinaryTree(root: TreeNode): Array[Array[Int]] = {
    val buffer = collection.mutable.Buffer[Int]()
    val output = collection.mutable.Buffer[Array[Int]]()

    def permute(node:TreeNode):Unit = {
      if (node != null) {
        if (node.left_ptr == null && node.right_ptr == null) {
          buffer.append(node.data)
          output.append(buffer.toArray)
          buffer.remove(buffer.length - 1)
        } else {
          buffer.append(node.data)
          permute(node.left_ptr)
          buffer.remove(buffer.length - 1)

          buffer.append(node.data)
          permute(node.right_ptr)
          buffer.remove(buffer.length - 1)
        }
      }
    }

    permute(root)
    output.toArray
  }

  //  println(postorder_traversal(tree).mkString(","))
  def postorder_traversal(root: TreeNode): Array[Int] = {
    val stack = collection.mutable.Stack[TreeNode]()
    val output = collection.mutable.Buffer[Int]()

    stack.push(root)
    while (!stack.isEmpty) {
      val node = stack.pop()

      if (node != null) {
        output.prepend(node.data)
        stack.push(node.left_ptr)
        stack.push(node.right_ptr)
      }
    }

    output.toArray
  }

  println(isBST(tree))
  def isBST(root: TreeNode): Boolean = {
    val buffer = collection.mutable.Buffer[Int]()

    def permute(node:TreeNode):Boolean = {
      if (node == null) return true

      val left = permute(node.left_ptr)
      if (!buffer.isEmpty) {
        if (!left || buffer.last > node.data) return false
        else buffer.remove(buffer.length-1)
      }
      buffer.append(node.data)
      val right = permute(node.right_ptr)

      left && right
    }

    permute(root)
  }

//  println(findSingleValueTrees(tree))
  def findSingleValueTrees(root: TreeNode): Int = {
    if (root == null) return 0

    var sum = 0
    def permute(node:TreeNode):Boolean = {
      if (node == null) return true

      val left = permute(node.left_ptr)
      val right = permute(node.right_ptr)

      if ((node.left_ptr == null || (left && node.left_ptr.data == node.data)) &&
        (node.right_ptr == null || (right && node.right_ptr.data == node.data))) {
          sum += 1
          true
        } else false
    }

    permute(root)
    sum
  }

}
