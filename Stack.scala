class Stack {

  private var top: Node = null

  def isEmpty: Boolean = {
    top == null
  }

  def pop(): Node = {
    if(isEmpty){
      return null
    }
    val n = top
    if(top.next == null) 
      //Only element in stack
      top = null
    else
      top = top.next
    n
  }

  def push(i: Int) {
    val n = new Node(i)
    if(!isEmpty) {
      n.next = top;
    }
    top = n
  }

  override def toString: String = {
    if(isEmpty){
      return "Empty Stack"
    }
    val sb = new StringBuilder()
    var n = top
    sb.append(n.data)
    while(n.next != null) {
      sb.append("," + n.next.data)
      n = n.next
    }
    sb.toString
  }

  

}

object StackTest {

  def main(args: Array[String]) {
    val s = new Stack()
    println(s)
    s.push(1)
    println(s)
    s.push(2)
    println(s)
    s.push(3)
    println(s)
    println("POPPED > " + s.pop())
    println(s)
    println("POPPED > " + s.pop())
    println(s)
    println("POPPED > " + s.pop())
    println(s)
  }

}
