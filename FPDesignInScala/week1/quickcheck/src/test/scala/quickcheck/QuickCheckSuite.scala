package quickcheck

import org.scalacheck.Prop
import org.scalacheck.Properties
import org.junit._

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Test.{check, Result, Failed, PropException}

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap {
  val x = -1
  val y = 0
  val a = this.insert(x,Nil)
  val a2 = this.insert(y,a)
  val b = this.insert(y,Nil)
  val b2 = this.insert(x,b)
  println(a2)
  println(b2)
  assert(a2 == b2)
}

class QuickCheckSuite {
  def checkBogus(p: Properties): Unit = {
    def fail = throw new AssertionError(
      s"A bogus heap should NOT satisfy all properties. Try to find the bug!")

    check(asProp(p))(identity) match {
      case r: Result => r.status match {
        case _: Failed         => () // OK: scalacheck found a counter example!
        case p: PropException  => p.e match {
          case e: NoSuchElementException => () // OK: the implementation throws NSEE
          case _ => fail
        }
        case _ => fail
      }
    }
  }

  /** Turns a `Properties` instance into a single `Prop` by combining all the properties */
  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq:_*)

  @Test def `Binomial heap satisfies properties. (5pts)`: Unit =
    Assert.assertTrue(
      check(asProp(new QuickCheckHeap with quickcheck.test.BinomialHeap))(identity).passed
    )

  @Test def `Bogus (1) binomial heap does not satisfy properties. (10pts)`: Unit =
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus1BinomialHeap)

  @Test def `Bogus (2) binomial heap does not satisfy properties. (10pts)`: Unit =
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus2BinomialHeap)

  @Test def `Bogus (3) binomial heap does not satisfy properties. (10pts)`: Unit =
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus3BinomialHeap)

  @Test def `Bogus (4) binomial heap does not satisfy properties. (10pts)`: Unit =
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap)

  @Test def `Bogus (5) binomial heap does not satisfy properties. (10pts)`: Unit =
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus5BinomialHeap)

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
