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

  def take_REC [A](as: List[A], n: Int): List[A] = as match
    case Cons(head, tail) if n > 0 => Cons(head, take_REC(tail, n - 1))
    case _ => Nil
  
  def take_FR[A](as: List[A], n: Int): List[A] =
    foldRight(as, (i: Int) => Nil: List[A], (a, acc) => 
      i => if i > 0 then Cons(a, acc(i - 1)) else Nil
    )(n)
  
  def take_FL[A](as: List[A], n: Int): List[A] =
    reverse(foldLeft(as, (Nil: List[A], n), (acc, head) =>
      if acc._2 > 0 then (Cons(head, acc._1), acc._2 - 1) else acc
    )._1)
    
  /*-------------------takeWhile------------------- */


  def takeWhile_REC[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(head, tail) => if f(head) then Cons(head, takeWhile_REC(tail, f)) else Nil

  def takeWhile_FL[A](as: List[A], f: A => Boolean): List[A] =
    reverse(foldLeft(as, (Nil: List[A], true), (acc, head) =>
      if acc._2 && f(head) then (Cons(head, acc._1), true) else (acc._1, false)
    )._1)

  def takeWhile_FR[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (head, acc) => if f(head) then Cons(head, acc) else Nil)

  /*-------------------splitAt------------------- */


  def splitAt_REC[A](as: List[A], n: Int): (List[A], List[A]) = if n == 0 then (Nil, as) else as match
    case Nil => (Nil, Nil)
    case Cons(head, tail) =>
      val (left, right) = splitAt_REC(tail, n - 1)
      (Cons(head, left), right)

  def splitAt_FL[A](as: List[A], n: Int): (List[A], List[A]) =
    val (left, right, _) = foldLeft(as, (Nil: List[A], Nil: List[A], n), (acc, head) =>
      if acc._3 > 0 then (Cons(head, acc._1), acc._2, acc._3 - 1)
      else (acc._1, Cons(head, acc._2), 0)
    )
    (reverse(left), reverse(right))

  def splitAt_FR[A](as: List[A], n: Int): (List[A], List[A]) =
    foldRight(as, (i: Int) => (Nil: List[A], Nil: List[A]), (a, acc) => 
      i => 
        if i > 0 then 
          val (l, r) = acc(i - 1)
          (Cons(a, l), r)
        else 
          val (_, r) = acc(0)
          (Nil, Cons(a, r))
    )(n)

  /*-------------------instersperse------------------- */


  def instersperse_REC[A](as: List[A], sep: A): List[A] = as match
    case Nil => Nil
    case Cons(head, Nil) => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, Cons(sep, instersperse_REC(tail, sep)))

  def instersperse_FL[A](as: List[A], sep: A): List[A] = as match
    case Nil => Nil
    case Cons(head, tail) => reverse(foldLeft(tail, Cons(head, Nil): List[A], (acc, a) => Cons(a, Cons(sep, acc))))

  def instersperse_FR[A](as: List[A], sep: A): List[A] = as match
    case Nil => Nil
    case Cons(head, tail) => Cons(head, foldRight(tail, Nil: List[A], (a, acc) => Cons(sep, Cons(a, acc))))

  /*-------------------partition------------------- */


  def partition_REC[A](as: List[A], f: A => Boolean): (List[A], List[A]) = as match
    case Nil => (Nil, Nil)
    case Cons(head, tail) =>
      val (left, right) = partition_REC(tail, f)
      if f(head) then (Cons(head, left), right) else (left, Cons(head, right))

  def partition_FL[A](as: List[A], f: A => Boolean): (List[A], List[A]) =
    val (left, right) = foldLeft(as, (Nil: List[A], Nil: List[A]), (acc, head) =>
      if f(head) then (Cons(head, acc._1), acc._2) else (acc._1, Cons(head, acc._2))
    )
    (reverse(left), reverse(right))

  def partition_FR[A](as: List[A], f: A => Boolean): (List[A], List[A]) =
    foldRight(as, (Nil: List[A], Nil: List[A]), (head, acc) =>
      if f(head) then (Cons(head, acc._1), acc._2) else (acc._1, Cons(head, acc._2))
    )

  /*-------------------last------------------- */


  def last_REC[A](as: List[A]): A = as match
    case Cons(head, Nil) => head
    case Cons(_, tail) => last_REC(tail)

  def last_FL[A](as: List[A]): A = as match
    case Cons(head, tail) => foldLeft(tail, head, (acc, a) => a)

  def last_FR[A](as: List[A]): A = as match
    case Nil => throw new Exception("Empty list")
    case _ => foldRight(as, None: Option[A], (a, acc) => acc.orElse(Some(a))).get

  /*-------------------minimumBy------------------- */


  def minimumBy_REC[A](as: List[A], score: A => Int): A = as match
    case Cons(head, Nil) => head
    case Cons(head, tail) =>
      val minTail = minimumBy_REC(tail, score)
      if score(head) <= score(minTail) then head else minTail

  def minimumBy_FL[A](as: List[A], score: A => Int): A = as match
    case Cons(head, tail) => foldLeft(tail, head, (acc, a) => if score(a) < score(acc) then a else acc)

  def minimumBy_FR[A](as: List[A], score: A => Int): A = as match
    case Cons(head, tail) => foldRight(tail, head, (a, acc) => if score(a) <= score(acc) then a else acc)

  /*-------------------compression------------------- */


  def compression_REC[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(head, Cons(next, tail)) if head == next => compression_REC(Cons(next, tail))
    case Cons(head, tail) => Cons(head, compression_REC(tail))

  def compression_FL[A](as: List[A]): List[A] =
    reverse(foldLeft(as, Nil: List[A], (acc, a) => acc match
      case Cons(h, _) if h == a => acc
      case _ => Cons(a, acc)
    ))

  def compression_FR[A](as: List[A]): List[A] =
    foldRight(as, Nil: List[A], (a, acc) => acc match
      case Cons(h, _) if h == a => acc
      case _ => Cons(a, acc)
    )

  /*-------------------interleave------------------- */


  def interleave_REC[A](a: List[A], b: List[A]): List[A] = (a, b) match
    case (Nil, _) => b
    case (_, Nil) => a
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah, Cons(bh, interleave_REC(at, bt)))

  def interleave_FL[A](a: List[A], b: List[A]): List[A] = 
    val (res, remB) = foldLeft(a, (Nil: List[A], b), (acc, x) => acc._2 match
      case Nil => (Cons(x, acc._1), Nil)
      case Cons(bh, bt) => (Cons(bh, Cons(x, acc._1)), bt)
    )
    append(reverse(res), remB)

  def interleave_FR[A](a: List[A], b: List[A]): List[A] = 
    foldRight(a, (remB: List[A]) => remB, (x, acc) => 
      remB => remB match
        case Nil => Cons(x, acc(Nil))
        case Cons(bh, bt) => Cons(x, Cons(bh, acc(bt)))
    )(b)
