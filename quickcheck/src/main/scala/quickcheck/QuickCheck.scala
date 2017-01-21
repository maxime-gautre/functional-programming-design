package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find min of one element") = forAll { a: Int =>
    val heap = insert(a, empty)
    findMin(heap) == a
  }

  property("find min of two elements") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == Math.min(a, b)
  }

  property("delete min of heap") = forAll { a: Int =>
    val heap = insert(a, empty)
    val result = deleteMin(heap)
    result == empty
  }


  property("findMin and deleteMin from a random heap should yield a sorted sequence") = forAll { entryHeap: H =>
    val result = getElements(entryHeap)
    result == result.sorted
  }

  property("min of meld heap") = forAll { (a: Int, b: Int) =>
    val heapA = insert(a, empty)
    val heapB = insert(b, empty)
    findMin(meld(heapA, heapB)) == Math.min(a, b)
  }

  property("minimum of melding two heaps") = forAll { (heapA: H, heapB: H) =>
    val result = meld(heapA, heapB)
    val min = findMin(result)
    val minA = findMin(heapA)
    val minB = findMin(heapB)

    min == Math.min(minA, minB)
  }

  property("melding three heaps") = forAll { (heapA: H, heapB: H, heapC: H) =>
    getElements(meld(heapA, meld(heapB, heapC))) == getElements(meld(meld(heapA, heapB), heapC))
  }

  def getElements(heap: H): List[A] = {
    if(isEmpty(heap)) Nil
    else findMin(heap) :: getElements(deleteMin(heap))
  }
}
