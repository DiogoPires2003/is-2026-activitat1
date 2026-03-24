import Queue.*

class QueueSuite extends munit.FunSuite {

  test("Queue basic operations") {
    val q0 = Queue.empty[Int]
    assertEquals(Queue.isEmpty(q0), true)

    val q1 = Queue.enqueue(q0, 1)
    assertEquals(Queue.isEmpty(q1), false)
    assertEquals(Queue.peek(q1), 1)

    val q2 = Queue.enqueue(q1, 2)
    val q3 = Queue.enqueue(q2, 3)

    assertEquals(Queue.toList(q3), List(1, 2, 3))

    val (v1, q4) = Queue.dequeue(q3)
    assertEquals(v1, 1)
    assertEquals(Queue.toList(q4), List(2, 3))

    val (v2, q5) = Queue.dequeue(q4)
    assertEquals(v2, 2)
    assertEquals(Queue.toList(q5), List(3))
  }
}
