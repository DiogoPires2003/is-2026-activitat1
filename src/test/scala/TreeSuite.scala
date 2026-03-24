import Tree.*

class TreeSuite extends munit.FunSuite {

  test("Tree.fold") {
    val input = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected = 6
    assertEquals(input.fold(identity, _ + _), expected)
  }

  test("Tree.mirror") {
    val root = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val expected = Branch(Branch(Leaf(3), Leaf(2)), Leaf(1))

    assertEquals(Tree.mirror(root), expected)
    assertEquals(Tree.mirror_REC(root), expected)
    assertEquals(Tree.mirror_FOLD(root), expected)
  }

  test("Tree.collect") {
    val root = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = List(1, 2, 3)

    assertEquals(Tree.collect(root), expected)
    assertEquals(Tree.collect_REC(root), expected)
    assertEquals(Tree.collect_FOLD(root), expected)
  }
}
