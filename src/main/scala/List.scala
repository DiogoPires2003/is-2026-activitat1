import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  // Already defined from the course

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil else Cons(as.head, apply(as.tail*))

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head), f)
      

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A], (acc, h) => Cons(h, acc))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a, b))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  // Exercises
  /*-------------------take------------------- */
  def take[A](as: List[A], n: Int): List[A] = ???

  def take_REC [A](as: List[A], n: Int): List[A] = as match
    case Cons(head, tail) if n > 0 => Cons(head, take_REC(tail, n - 1))
    case _ => Nil
  
  def take_FR[A](as: List[A], n: Int): List[A] = ???
   
  
  def take_FL[A](as: List[A], n: Int): List[A] = ???
    
  /*-------------------takeWhile------------------- */

  def takeWhile[A](as: List[A], f: A => Boolean): List[A] = ???

  def takeWhile_REC[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(head, tail) => if f(head) then Cons(head, takeWhile_REC(tail, f)) else Nil

  def takeWhile_FL[A](as: List[A], f: A => Boolean): List[A] = ???

  def takeWhile_FR[A](as: List[A], f: A => Boolean): List[A] = ???

  /*-------------------splitAt------------------- */

  def splitAt[A](as: List[A], n: Int): (List[A], List[A]) = ???

  def splitAt_REC[A](as: List[A], n: Int): (List[A], List[A]) = if n == 0 then (Nil, as) else as match
    case Nil => (Nil, Nil)
    case Cons(head, tail) =>
      val (left, right) = splitAt_REC(tail, n - 1)
      (Cons(head, left), right)

  def splitAt_FL[A](as: List[A], n: Int): (List[A], List[A]) = ???

  def splitAt_FR[A](as: List[A], n: Int): (List[A], List[A]) = ???

  /*-------------------instersperse------------------- */

  def instersperse[A](as: List[A], sep: A): List[A] = ???

  def instersperse_REC[A](as: List[A], sep: A): List[A] = as match
    case Nil => Nil
    case Cons(head, Nil) => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, Cons(sep, instersperse_REC(tail, sep)))

  def instersperse_FL[A](as: List[A], sep: A): List[A] = ???

  def instersperse_FR[A](as: List[A], sep: A): List[A] = ???

  /*-------------------partition------------------- */

  def partition[A](as: List[A], f: A => Boolean): (List[A], List[A]) = ???

  def partition_REC[A](as: List[A], f: A => Boolean): (List[A], List[A]) = as match
    case Nil => (Nil, Nil)
    case Cons(head, tail) =>
      val (left, right) = partition_REC(tail, f)
      if f(head) then (Cons(head, left), right) else (left, Cons(head, right))

  def partition_FL[A](as: List[A], f: A => Boolean): (List[A], List[A]) = ???

  def partition_FR[A](as: List[A], f: A => Boolean): (List[A], List[A]) = ???

  /*-------------------last------------------- */

  def last[A](as: List[A]): A = ???

  def last_REC[A](as: List[A]): A = as match
    case Cons(head, Nil) => head
    case Cons(_, tail) => last_REC(tail)

  def last_FL[A](as: List[A]): A = ???

  def last_FR[A](as: List[A]): A = ???

  /*-------------------minimumBy------------------- */

  def minimumBy[A](as: List[A], score: A => Int): A = ???

  def minimumBy_REC[A](as: List[A], score: A => Int): A = as match
    case Cons(head, Nil) => head
    case Cons(head, tail) =>
      val minTail = minimumBy_REC(tail, score)
      if score(head) <= score(minTail) then head else minTail

  def minimumBy_FL[A](as: List[A], score: A => Int): A = ???

  def minimumBy_FR[A](as: List[A], score: A => Int): A = ???

  /*-------------------compression------------------- */

  def compression[A](as: List[A]): List[A] = ???

  def compression_REC[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(head, tail) => Cons(head, compression_REC(tail).filter(_ != head))

  def compression_FL[A](as: List[A]): List[A] = ???

  def compression_FR[A](as: List[A]): List[A] = ???

  /*-------------------interleave------------------- */

  def interleave[A](a: List[A], b: List[A]): List[A] = ???

  def interleave_REC[A](a: List[A], b: List[A]): List[A] = (a, b) match
    case (Nil, _) => b
    case (_, Nil) => a
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah, Cons(bh, interleave_REC(at, bt)))

  def interleave_FL[A](a: List[A], b: List[A]): List[A] = ???

  def interleave_FR[A](a: List[A], b: List[A]): List[A] = ???
