class Node(private var _data: Int) {

  private var _next: Node = null

  def data = _data
  
  def data_= (newData: Int) = _data = newData
  
  def next = _next

  def next_= (newNext: Node) = _next = newNext

  override def toString: String = {
    "Node id: " + data 
  }

}
