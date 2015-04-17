package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)

    findMin(insert(m, h)) == m
  }

  property("checkMinfOf2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a1, insert(a2, empty))
    val min = if (a1 < a2) a1 else a2

    findMin(h) == min
  }

  property("meld1") = forAll { (a1: Int, a2: Int) =>
    val h1 = insert(a1, empty)
    val h2 = insert(a2, empty)
    val h = meld(h1, h2)
    val min = if (a1 < a2) a1 else a2

    findMin(h) == min
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2
    val h = meld(h1, h2)

    findMin(h) == min
  }

  property("meld3") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val max = if (min1 > min2) min1 else min2
    val h = deleteMin(meld(h1, h2))

    findMin(h) == max
  }

  property("deleteMin1") = forAll { a1: Int =>
    val h1 = insert(a1, empty)
    val h2 = deleteMin(h1)

    isEmpty(h2)
  }

  property("deleteMin2") = forAll { (a1: Int, a2: Int) =>
    val h1 = insert(a1, insert(a2, empty))
    val max = if (a1 > a2) a1 else a2

    val h2 = deleteMin(h1)

    findMin(h2) == max
  }

  property("ordered1") = forAll { h: H =>
    def remMin(q: H, acc: List[Int]): List[Int] = {
      if (isEmpty(q)) acc
      else
        findMin(q) :: remMin(deleteMin(q), acc)
    }

    val xs = remMin(h, Nil)
    xs == xs.sorted
  }



}
