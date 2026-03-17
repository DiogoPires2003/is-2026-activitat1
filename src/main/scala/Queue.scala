case class Queue[+A](in: List[A], out: List[A])

object Queue:

  // Exercises

  def empty[A]: Queue[A] = ???

  def isEmpty[A](q: Queue[A]): Boolean = ???

  def enqueue[A](q: Queue[A], a: A): Queue[A] = ???

  // If empty invoke sys.error
  def dequeue[A](q: Queue[A]): (A, Queue[A]) = ???

  // If empty invoke sys.error
  def peek[A](q: Queue[A]): A = ???

  def toList[A](q: Queue[A]): List[A] = ???
