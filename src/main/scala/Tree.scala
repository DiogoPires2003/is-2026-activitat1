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

  def mirror[A](t: Tree[A]): Tree[A] = ???

  def collect[A](t: Tree[A]): List[A] = ???
