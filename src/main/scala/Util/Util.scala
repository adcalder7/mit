package Util

object Util {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  def buildTree() = {
    val head = new TreeNode(1)

    head.left = new TreeNode(2)
    head.right = new TreeNode(2)

    head.left.left = new TreeNode(3)
    head.left.right = new TreeNode(4)

    head.right.left = new TreeNode(3)
    head.right.right = new TreeNode(4)

    head
  }

  def printTreeInOrder(root: TreeNode): TreeNode = {
    if (root == null) return null

    printTreeInOrder(root.left)
    print(root.value + ", ")
    printTreeInOrder(root.right)

    root
  }

  def printTreePreOrder(root: TreeNode): TreeNode = {
    if (root == null) return null

    print(root.value + ", ")
    printTreeInOrder(root.left)
    printTreeInOrder(root.right)

    root
  }

  def printTreePosOrder(root: TreeNode): TreeNode = {
    if (root == null) return null

    printTreeInOrder(root.left)
    printTreeInOrder(root.right)
    print(root.value + ", ")

    root
  }

  def printLinkedList(h: ListNode): Unit = {
    var next = h
    while (next != null) {
      print(next.x + ", ")
      next = next.next
    }
    println()
  }

  def getLinkedList(start: Int = 1, end: Int = 10, skip: Int = 1): ListNode = {
    val head: ListNode = new ListNode(start)
    var iterator: ListNode = head
    for (i <- start + 1 to end by skip) {
      iterator.next = new ListNode(i)
      iterator = iterator.next
    }
    head
  }

}
