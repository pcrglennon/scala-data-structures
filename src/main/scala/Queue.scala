class Queue {

  private var head: Node = null
  private var tail: Node = null

  def isEmpty: Boolean = {
    head == null
  }

  def enQueue(i: Int) {
    val n = new Node(i)
    if(isEmpty) {
      head = n
      tail = n
    }
    else {
      tail.next = n
      tail = n
    }
  }

  def deQueue(): Node = {
    if(isEmpty) {
      return null
    }
    val n = head
    if(tail == head) {
      //Only element in queue
      head = null
      tail = null
    }
    else {
      head = head.next
    }
    n
  }

  override def toString: String = {
    if(isEmpty) {
      return "EMPTY QUEUE"
    }
    val sb = new StringBuilder()
    sb.append("QUEUE HEAD >> " )
    var n = head
    sb.append(head.data)
    while(n.next != null) {
      sb.append("," + n.next.data)
      n = n.next
    }
    sb.append(" << TAIL")
    sb.toString
  }

}
/*
object QueueTest {

  def main(args: Array[String]) {
    val q = new Queue()
    println(q)
    println("DeQueued " + q.deQueue())
    q.enQueue(1)
    println(q)
    q.enQueue(2)
    println(q)
    q.enQueue(3)
    println(q)
    q.enQueue(1000)
    println(q)
    println("DeQueued " + q.deQueue())
    println(q)
    println("DeQueued " + q.deQueue())
    println(q)
    println("DeQueued " + q.deQueue())
    println(q)
  }

}
*/
