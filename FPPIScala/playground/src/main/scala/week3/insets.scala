package week3 {

  object insets {
    def main(args: Array[String]): Unit = {
      val t1 = new NonEmpty(3, new Empty, new Empty)
      println(t1)
      val t2 = t1 incl 4
      println(t2)
      val t3 = t2 union new NonEmpty(5, new Empty, new Empty)
      println(t3)
      val t4 = t3 incl 1 incl 2
      println(t4)
      val t5 = t3 union t4
      println(t5)
    }

    abstract class IntSet {
      def incl(x: Int): IntSet

      def contains(x: Int): Boolean

      def union(other: IntSet): IntSet
    }

    class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
      def contains(x: Int): Boolean =
        if (x < elem) left contains x
        else if (x > elem) right contains x
        else true

      def incl(x: Int): IntSet = {
        if (x < elem) new NonEmpty(elem, left incl x, right)
        else if (x > elem) new NonEmpty(elem, left, right incl x)
        else this
      }

      def union(other: IntSet): IntSet = {
        ((left union right) union other) incl elem
      }

      override def toString: String = "{" + left + elem + right + "}"
    }

    class Empty extends IntSet {
      def contains(x: Int): Boolean = false

      def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

      def union(other: IntSet): IntSet = other

      override def toString = "."
    }

  }

}
