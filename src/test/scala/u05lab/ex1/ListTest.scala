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

