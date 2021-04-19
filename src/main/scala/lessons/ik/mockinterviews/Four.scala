package lessons.ik.mockinterviews

/*
# == == == == == == == == == == == == == == == == == == == == == =
#
# Given the root of a Binary Search Tree (BST),
# "convert" it to a Greater Tree such that every key of the original BST is
# changed to the original key plus sum of all keys greater than the original key in BST.
#
# Example 1:
#
# Input: TreeNode(4)
#             4
#           /   \
#          1      6
#        /  \    / \
#       0    2   5   7
#             \       \
#              3       8
# Output: void                   right ->
#             30
#           /   \
#          36     21
#        /  \    / \
#       36  35  26   15
#             \       \
#             33       8

1)
....
...
..
TC O(N), SC O(N)

2) ...
...
...
TC O(N), SC O(1),,,, rec stack space...O(H) logN N
var total = 0
permute(node) {
    total += node.val
    permute(node.right)
    node.val = total
    permute(node.left)
}

0- 1- 2- 3- 4- 5- 6- 7- 8 <<= list[TN]? TN
j  j

36-36-35-33-30-26-21-15-8
i  i


#
# Constraints:
#
# The number of nodes in the tree is in the range [1, 100].
# 0 <= Node.val <= 100
# All the values in the tree are unique.
# root is guaranteed to be a valid binary search tree.
#
# == == == == == == == == == == == == == == == == == == == == == =



object Solution extends App {

  // 0- 1- 2- 3- 4- 5- 6- 7- 8
  /*
  # Input: TreeNode(4)
#             4 (30)
#           /   \
#          1      6 (21)
#        /  \    / \
#       0    2   5   7 (15)
#             \  (26) \
#              3       8 (8)
  */
  def bstCovert(root:TreeNode): Unit = {
    var total = 0
    def permute(node:TreeNode): Unit = {
      if (node != null) {
        permute(node.right)
        total = total + node.val
        node.val = total
        permute(node.left)
      }
    }
    permute(root)
  }

  println("here")

}

/*
# == == == == == == == == == == == == == == == == == == == == == =
#
# Integer array of size N , all the elements in the array are between 0 and N,
# if k is an element then 0 <= k < N
# N = 5 elements allowed are  0,1,2,3,4
# It can have lot of duplicates.
# Not sorted.
#
# ===> Input array is mutable as long as you can restore it back. <===
#
# Input: [4,2,4+N+N+N,2,2+N+N]
# Output:
# >> 4 appeared 2 times
# >> 2 appeared 3 times
#

// Number lower than N
4->2->4->2->2

[4,2,4,2,2] | [2,2,4,2,4]

x appeared y times

- Grouping
- Sorting
- Swapping*
- Mirror
- Matrix
- Recursion*

# == == == == == == == == == == == == == == == == == == == == == =
*/
def get1(nums):
N = len(nums)
for n in nums:
original = n % N
nums[original] += N

for i, n in enumerate(nums):
freq = n // N
if freq == 0: continue
print(i + " appeared " + freq + " times.")

for i, n in enumerate(nums):
nums[i] %= N

return
// T=C*O(n), S=O(1)




*/









