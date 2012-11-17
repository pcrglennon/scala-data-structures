class BST {

  private var _root: BSTNode = null
  private var chooseSuccessor = true

  def root = _root

  def root_= (newRoot: BSTNode) = _root = newRoot

  /*
   * Public find method - begins at root
   */
  def findNode(toFind: Int): BSTNode = {
    findNode(root, toFind)
  }

  /*
   * Internal recursive find method
   */
  private def findNode(curNode: BSTNode, toFind: Int): BSTNode = {
    if(curNode == null) return null
    if(toFind == curNode.data) return curNode
    if(toFind < curNode.data) {
      findNode(curNode.left, toFind)
    } else {
      findNode(curNode.right, toFind)
    }
  }
  
  /*
   * Public traversal method - begins from root
   */
  def inOrderTraverse: String = {
    val sb = new StringBuilder()
    inOrderTraverse(root, sb)
    sb.toString
  }

  /*
   * Internal recursive traverse method
   */
  private def inOrderTraverse(curNode: BSTNode, sb: StringBuilder) {
    if(curNode == null) {
      return
    }
    inOrderTraverse(curNode.left, sb)
    sb.append(curNode.data + " ")
    inOrderTraverse(curNode.right, sb)
  }
  
  /*
   * Public insert method - begins at root
   */
  def insertNode(data: Int) {
    if(isEmpty) {
      root = new BSTNode(data)
    }
    else {
      insertNode(root, data)
    }
  }
    
  /*
   * Internal recursive insertion method
   */
  private def insertNode(curNode: BSTNode, data: Int) {
    if(data < curNode.data) {
      if(curNode.left == null) {
	curNode.left = new BSTNode(data, curNode)
      } else {
	insertNode(curNode.left, data)
      }
    } else {
      if(curNode.right == null) {
	curNode.right = new BSTNode(data, curNode)
      } else {
	insertNode(curNode.right, data)
      }
    }
  }
  
  /*
   * Public delete method
   */
  def deleteNode(data: Int): BSTNode = {
    val toDelete = findNode(data)
    if(toDelete == null) {
      return null
    } else if(toDelete == root) {
      deleteRoot()
    } else {
      childCase(toDelete) match {
	case 1 => removeFromParent(toDelete)
	case 2 => swapWithChild(toDelete, toDelete.right)
	case 3 => swapWithChild(toDelete, toDelete.left)
	case _ => return deleteWithTwoChildren(toDelete)
      }
    }
    return toDelete
  }

  /*
   * Checks the number of children for a node
   *
   * Returns 1 if no children
   * Returns 2 if only a right child
   * Returns 3 if only a left child
   * Returns 4 otherwise (there are 2 children)
   */
  def childCase(toDelete: BSTNode): Int = {
    if(toDelete.left == null && toDelete.right == null) 1
    else if(toDelete.left == null && toDelete.right != null) 2
    else if(toDelete.left != null && toDelete.right == null) 3
    else 4
  }

  /*
   * Poorly-named method for handling deletion of a node w/ 2 children
   */
  private def deleteWithTwoChildren(toDelete: BSTNode): BSTNode = {
    val replace = if(chooseSuccessor) getSuccessor(toDelete) else getPredecessor(toDelete)
    val replaceData = replace.data
    replace.data = toDelete.data
    toDelete.data = replaceData
    childCase(replace) match {
      case 1 => removeFromParent(replace)
      case 2 => swapWithChild(replace, replace.right)
      case 3 => swapWithChild(replace, replace.left)
      case _ => {
	println("UH OH, SOMETHING'S WRONG")
      }
    }
    //Flip the value of chooseSuccessor
    chooseSuccessor = if(chooseSuccessor) false else true
    return replace
  }

  /*
   * Method to handle deletion of node w/ 1 child
   */
  private def swapWithChild(toDelete: BSTNode, child: BSTNode) {
    if(toDelete.parent.left == toDelete) {
      toDelete.parent.left = child
      child.parent = toDelete.parent
    }
    else {
      toDelete.parent.right = child
      child.parent = toDelete.parent
    }
    toDelete.parent = null
  }
  
  /*
   * Method to handle deletion of node w/ no chilren
   */
  def removeFromParent(toDelete: BSTNode) {
    if(toDelete.parent.left == toDelete)
      toDelete.parent.left = null
    else
      toDelete.parent.right = null
    toDelete.parent = null
  }
  
  /*
   * Get the in-order successor of a node
   */
  private def getSuccessor(parent: BSTNode): BSTNode = {
    var n = parent.right
    while(n.left != null) {
      n = n.left
    }
    n
  }

  /*
   * Get the in-order predecessor of a node
   */
  private def getPredecessor(parent: BSTNode): BSTNode = {
    var n = parent.left
    while(n.right != null) {
      n = n.right
    }
    n
  }
  
  /*
   * Special case for deletion - node to be deleted is the root
   *
   * TODO - remove this, alter the root to allow it to be handled like any other node
   */
  private def deleteRoot() {
    childCase(root) match {
      case 1 => root = null
      case 2 => {
	root = root.right
	root.parent = null
      }
      case 3 => {
	root = root.left
	root.parent = null
      }
      case _ => {

      }
    }
  }

  /*
   * Returns true if the tree is empty
   */
  def isEmpty: Boolean = {
    root == null
  }
}

/*
 * Test method, building this example tree:
 *   1
 *    \
 *     2
 *      \
 *       6
 *      / \
 *     4   7
 *    / \
 *   3   5
 *
 * Deletes 1 (the root), then 6, then 7, and then 5
 *
 * Commented out by default
 */

/*
object BSTTest {

  def main(args: Array[String]) {
    val bst = new BST()
    bst.insertNode(1)
    bst.insertNode(2)
    bst.insertNode(6)
    bst.insertNode(4)
    bst.insertNode(7)
    bst.insertNode(3)
    bst.insertNode(5)
    println(bst.inOrderTraverse)
    var root = bst.root
    println(root + " LC >> " + root.left + " RC >> " + root.right + " parent >> " + root.parent)
    println("Deleting root >> " + bst.deleteNode(1))
    var find2 = bst.findNode(2)
    println(find2 + " LC >> " + find2.left + " RC >> " + find2.right + " parent >> " + find2.parent)
    println(bst.inOrderTraverse)
    var find4 = bst.findNode(4)
    var find7 = bst.findNode(7)
    var find6 = bst.findNode(6)
    println(find4 + " LC >> " + find4.left + " RC >> " + find4.right + " parent >> " + find4.parent)
    println(find7 + " + parent " + find7.parent)
    println(find6 + " LC >> " + find6.left + " RC >> " + find6.right + " parent >> " + find6.parent)
    println("Deleting " + bst.deleteNode(6))
    println(bst.inOrderTraverse)
    find4 = bst.findNode(4)
    println(find4 + " LC >> " + find4.left + " RC >> " + find4.right + " parent >> " + find4.parent)
    find7 = bst.findNode(7)
    println(find7 + " LC >> " + find7.left + " RC >> " + find7.right + " parent >>  " + find7.parent)
    println("Deleting " + bst.deleteNode(7))
    find4 = bst.findNode(4)
    println(find4 + " LC >> " + find4.left + " RC >> " + find4.right + " parent >> " + find4.parent)
    println(bst.inOrderTraverse)
    println("Deleting " + bst.deleteNode(5))
    println(bst.inOrderTraverse)
    var find3 = bst.findNode(3)
    println(find3 + " LC >> " + find3.left + " RC >> " + find3.right + " parent >>  " + find3.parent)
  }
}
*/
