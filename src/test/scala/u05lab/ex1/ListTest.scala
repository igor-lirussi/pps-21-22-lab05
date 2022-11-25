package u05lab.ex1

import org.junit.Test
import u05lab.ex1.List.*
class ListTest :

  val reference = List(1, 2, 3, 4)
  val list = List(1, 2, 3, 4, 5)

  @Test
  def testZipRight(): Unit =
    assert(list.zipRight == List((1, 0), (2, 1), (3, 2), (4, 3), (5, 4)))
    assert(reference.zipRight == List((1, 0), (2, 1), (3, 2), (4, 3)))
    assert(list.zipRightRec == List((1, 0), (2, 1), (3, 2), (4, 3), (5, 4)))
    assert(reference.zipRightRec == List((1, 0), (2, 1), (3, 2), (4, 3)))
    assert(list.zipRight1 == List((1, 0), (2, 1), (3, 2), (4, 3), (5, 4)))
    assert(reference.zipRight1 == List((1, 0), (2, 1), (3, 2), (4, 3)))

  @Test
  def testPartition(): Unit =
    assert(reference.partition(_ % 2 == 0) == (List(2, 4), List(1, 3)))
    assert(reference.partition2(_ % 2 == 0) == (List(2, 4), List(1, 3)))
    assert(reference.partitionRec(_ % 2 == 0) == (List(2, 4), List(1, 3)))

  @Test
  def testSpan(): Unit =
    assert(reference.span(_ % 2 != 0) == (List(1), List(2, 3, 4)))
    assert(reference.span(_ < 3) ==  (List(1, 2), List(3, 4)))
    assert(reference.spanRec(_ % 2 != 0) == (List(1), List(2, 3, 4)))
    assert(reference.spanRec(_ < 3) == (List(1, 2), List(3, 4)))

  @Test
  def testReduce(): Unit =
    assert(reference.reduce(_ + _) == 10)