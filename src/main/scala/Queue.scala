case class Queue[+A](in: List[A], out: List[A])

object Queue:

  // Exercises

  def empty[A]: Queue[A] = Queue(List.Nil, List.Nil)

  def isEmpty[A](q: Queue[A]): Boolean = q.in == List.Nil && q.out == List.Nil

  def enqueue[A](q: Queue[A], a: A): Queue[A] = Queue(List.Cons(a, q.in), q.out)

  // If empty invoke sys.error
  def dequeue[A](q: Queue[A]): (A, Queue[A]) = q.out match
    case List.Nil => q.in match
      case List.Nil => sys.error("empty")
      case _ =>
        val rev = List.reverse(q.in)
        rev match
          case List.Cons(h, t) => (h, Queue(List.Nil, t))
          case List.Nil => sys.error("empty")
    case List.Cons(h, t) => (h, Queue(q.in, t))

  // If empty invoke sys.error
  def peek[A](q: Queue[A]): A = q.out match
    case List.Nil => q.in match
    case List.Nil => sys.error("empty")
      case _ =>
        List.last_REC(q.in) // The last element of in is the first element of out
    case List.Cons(h, _) => h

  def toList[A](q: Queue[A]): List[A] = List.append(q.out, List.reverse(q.in))
