class BSTNode(private var _data: Int, private var _parent: BSTNode = null) {

  private var _left: BSTNode = null
  private var _right: BSTNode = null

  def data = _data

  def data_= (newData: Int) = _data = newData

  def parent = _parent

  def parent_= (newParent: BSTNode) = _parent = newParent

  def left = _left
  
  def left_= (newLeft: BSTNode) = _left = newLeft

  def right = _right

  def right_= (newRight: BSTNode) = _right = newRight

  override def toString: String = {
    "BSTNode - data[%d]".format(data)
  }

}
