class ListSuite extends munit.FunSuite {

  test("List.reverse") {
    val input = List(1,2,3,4,5)
    val expected = List(5,4,3,2,1)
    assertEquals(List.reverse(input), expected)
  }

  test("List.take") {
    val input = List(1, 2, 3, 4, 5)
    val expected = List(1, 2, 3)
    assertEquals(List.take_REC(input, 3), expected)
    assertEquals(List.take_FL(input, 3), expected)
    assertEquals(List.take_FR(input, 3), expected)
  }

  test("List.takeWhile") {
    val input = List(2, 4, 6, 1, 8)
    val expected = List(2, 4, 6)
    val f: Int => Boolean = _ % 2 == 0
    assertEquals(List.takeWhile_REC(input, f), expected)
    assertEquals(List.takeWhile_FL(input, f), expected)
    assertEquals(List.takeWhile_FR(input, f), expected)
  }

  test("List.splitAt") {
    val input = List(1, 2, 3, 4, 5)
    val expected = (List(1, 2), List(3, 4, 5))
    assertEquals(List.splitAt_REC(input, 2), expected)
    assertEquals(List.splitAt_FL(input, 2), expected)
    assertEquals(List.splitAt_FR(input, 2), expected)
  }

  test("List.instersperse") {
    val input = List(1, 2, 3)
    val expected = List(1, 0, 2, 0, 3)
    assertEquals(List.instersperse_REC(input, 0), expected)
    assertEquals(List.instersperse_FL(input, 0), expected)
    assertEquals(List.instersperse_FR(input, 0), expected)
  }

  test("List.partition") {
    val input = List(1, 2, 3, 4, 5, 6)
    val expected = (List(2, 4, 6), List(1, 3, 5))
    val f: Int => Boolean = _ % 2 == 0
    assertEquals(List.partition_REC(input, f), expected)
    assertEquals(List.partition_FL(input, f), expected)
    assertEquals(List.partition_FR(input, f), expected)
  }

  test("List.last") {
    val input = List(1, 2, 3, 4)
    assertEquals(List.last_REC(input), 4)
    assertEquals(List.last_FL(input), 4)
    assertEquals(List.last_FR(input), 4)
  }

  test("List.minimumBy") {
    val input = List(3, 1, 4, 1, 5)
    val expected = 1
    val f: Int => Int = identity
    assertEquals(List.minimumBy_REC(input, f), expected)
    assertEquals(List.minimumBy_FL(input, f), expected)
    assertEquals(List.minimumBy_FR(input, f), expected)
  }

  test("List.compression") {
    val input = List(1, 1, 2, 3, 3, 3, 2, 2)
    val expected = List(1, 2, 3, 2)
    assertEquals(List.compression_REC(input), expected)
    assertEquals(List.compression_FL(input), expected)
    assertEquals(List.compression_FR(input), expected)
  }

  test("List.interleave") {
    val a = List(1, 3, 5)
    val b = List(2, 4, 6)
    val expected = List(1, 2, 3, 4, 5, 6)
    assertEquals(List.interleave_REC(a, b), expected)
    assertEquals(List.interleave_FL(a, b), expected)
    assertEquals(List.interleave_FR(a, b), expected)
  }
}
