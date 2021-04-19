package lessons.ik.mockinterviews

object DeleteTreeLeaves {
  import collection.mutable.Buffer

  case class TreeNode(v:Int) {
    val value = v
    var left:TreeNode = null
    var right:TreeNode = null
  }

  def deleteNodes(root:TreeNode): (TreeNode, List[Int]) = {
    if (root == null) (null, List.empty)
    else if (root.left == null && root.right == null) {
      (null, List(root.value))
    } else {
      val buf = Buffer[Int]()
      // Return true if its a leaf
      def permute(node:TreeNode): Boolean = {
        if (node == null) return true
        else if (node.left == null && node.right == null) {
          buf.append(node.value)
          return true
        } else {
          // We don't want to delete if its not a leaf
          if (permute(node.left)) node.left = null
          if (permute(node.right)) node.right = null
        }
        false
      }
      permute(root)
      (root, buf.toList)
    }
  }

  def findLeaves(root: TreeNode): List[List[Int]] = {
    var root2 = root
    val buf = Buffer[List[Int]]()
    while (root2 != null) {
      val (newRoot, deletedNodes) = deleteNodes(root)
      root2 = newRoot
      buf.append(deletedNodes)
    }
    buf.toList
  }
}
