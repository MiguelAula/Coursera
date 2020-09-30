package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def extractMins(h: H, acc: List[A] ): List[A] = if (isEmpty(h)) acc.reverse else extractMins(deleteMin(h),findMin(h) :: acc)

  /**
    * Adding a single element to an empty heap, and then removing this element, should yield the element in question.
    */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * For any heap, adding the minimal element, and then finding it, should return the element in question
    */
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b,insert(a, empty))
    findMin(h) == Math.min(a,b)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
    */
  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting min.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("gen1") = forAll { (h: H) =>
    val l = extractMins(h,List())
    l == l.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("meld") = forAll { (h1: H, h2: H) =>
    val m = meld(h1,h2)
    findMin(m) == Math.min(findMin(h1),findMin(h2))
  }

  /** -ADDED-
    * Given any heap h, if you insert an element a into h, the minimum of the resulting heap should be the minimum of a and findMin(h)
    */
  property("min4") = forAll { (a: Int, h: H) =>
    val h2 = insert(a,h)
    findMin(h2) == Math.min(findMin(h),a)
  }

  /** -ADDED-
    * Given any 2 heaps, check they are still sorted after melding
    */
  property("sorted meld") = forAll { (h1: H, h2: H) =>
    val m = meld(h1,h2)
    val l = extractMins(m,List())
    l == l.sorted
  }

  /** -ADDED-
    * Check meld empty and any h, is h
    */
  property("meld3") = forAll { (h: H) =>
    val m = meld(h,empty)
    m == h
  }

  /** -ADDED-
    * Check insert a and b to h is same as b and a (commutative)
    */
  property("commutativeInsert") = forAll { (a: Int, b: Int) =>
    insert(b,insert(a, empty)) == insert(a,insert(b,empty))
  }
  /*
  property("commutativeInsert2") = forAll { (h: H, a: Int, b: Int) =>
     insert(b,insert(a,h)) == insert(a,insert(b,h))
  }
  */
  /** -ADDED-
    * Check insert a and b to h is same as b and a (commutative)
    */
  property("commutativeMeld") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a,empty)
    val h2 = insert(b,empty)
    meld(h1,h2) == meld(h2,h1)
  }
  /*
  property("commutativeMeld2") = forAll { (h1: H, h2: H) =>
    meld(h1,h2) == meld(h2,h1)
  }
  */
}
