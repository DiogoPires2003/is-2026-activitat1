enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  // Already defined from the course

  def fold[B](f: A => B, g: (B, B) => B): B =
    this match
      case Tree.Leaf(value) => f(value)
      case Tree.Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

object Tree:

  // Exercises

  def mirror_REC[A](t: Tree[A]): Tree[A] = t match
    case Tree.Leaf(v) => Tree.Leaf(v)
    case Tree.Branch(l, r) => Tree.Branch(mirror_REC(r), mirror_REC(l))

  def mirror_FOLD[A](t: Tree[A]): Tree[A] =
    t.fold[Tree[A]](
      v => Tree.Leaf(v),
      (l, r) => Tree.Branch(r, l)
    )

  def mirror[A](t: Tree[A]): Tree[A] = mirror_FOLD(t)

  def collect_REC[A](t: Tree[A]): List[A] = t match
    case Tree.Leaf(v) => List.Cons(v, List.Nil)
    case Tree.Branch(l, r) => List.append(collect_REC(l), collect_REC(r))

  def collect_FOLD[A](t: Tree[A]): List[A] =
    t.fold[List[A]](
      v => List.Cons(v, List.Nil),
      (l, r) => List.append(l, r)
    )

  def collect[A](t: Tree[A]): List[A] = collect_FOLD(t)
