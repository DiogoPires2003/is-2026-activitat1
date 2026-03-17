class ListSuite extends munit.FunSuite {

  test("List.reverse") {
    val input = List(1,2,3,4,5)
    val expected = List(5,4,3,2,1)
    assertEquals(List.reverse(input), expected)
  }
}
