package week4

object list {
  def main(args: Array[String]): Unit = {
    val l1 = List(5)
    println(l1)
    val l2 = List(5,6,7)
    println(l2)
    val l3 = l2.nth(2)
    println(l3)
  }

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def append(x: T): List[T]
    def nth(n: Int): T
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
    def append(x: T): Cons[T] = {
      new Cons(head,tail.append(x))
    }
    def nth(n: Int): T = {
      if (n == 0) head
      else tail.nth(n-1)
    }
    override def toString: String = head + "->" + tail.toString
  }
  class Nil[T] extends List[T] {
    def isEmpty = true
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.tail")
    def append(x: T) = new Cons(x,this)
    def nth(n: Int) = throw new IndexOutOfBoundsException()

    override def toString = "."
  }

  object List {
    // list(1,2) = List.apply(1,2)
    def apply[T](x1: T): List[T] = new Cons(x1,new Nil)
    def apply[T](x1: T, x2: T): List[T] = List(x1).append(x2)
    def apply[T](x1: T, x2: T, x3: T): List[T] = List(x1).append(x2).append(x3)
    def apply[T]() = new Nil
  }
}
