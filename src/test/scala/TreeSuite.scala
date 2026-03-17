import Tree.*

class TreeSuite extends munit.FunSuite {

  test("Tree.fold") {
    val input = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected = 6
    assertEquals(input.fold(identity, _ + _), expected)
  }
}
