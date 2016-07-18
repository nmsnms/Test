//check

trait Queue[+T] {
  def isEmpty: Boolean
  def enQueue[B >: T](t: B): Queue[B]
  def deQueue(): Queue[T]
  def head: Option[T]
}

object Queue {

  def empty[T]: Queue[T] =  new QueueHelper[T](Nil,Nil)
  def apply[T](xs : T*) : Queue[T] = new QueueHelper[T](xs.toList, Nil)

  private class QueueHelper[T](private val in : List[T], private val out: List[T]) extends Queue[T]{

    private def get = if (in.isEmpty) new QueueHelper(out.reverse, Nil) else this

    def isEmpty: Boolean = in.isEmpty && out.isEmpty
    def enQueue[B >: T](t: B) = new QueueHelper[B](in, t :: out)
    def deQueue(): Queue[T] = {
      val queue = get
      if (!queue.isEmpty)
        new QueueHelper[T](queue.in.tail ,queue.out)
      else
        throw new NoSuchElementException("Empty queue")
    }
    def head : Option[T] = get.in.headOption

    override def toString: String = {
      val queue = in ::: out.reverse
      queue.mkString("Queue(", ", ", ")")
    }
  }
}