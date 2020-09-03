package week5

object ListMethods {
  def main(args: Array[String]): Unit = {
    val l1 = removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
    println(l1)
    //val l2 = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    //println(l2)
    val l3 = merge(List(1,3,5),List(2))
    println(l3)
  }
/*
  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case List(x) => x match {
      case List => x
      case _ => List(x)
    }
    case x :: xs => x :: flatten(xs)
  }*/

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

  def removeAt_2[T](n: Int, xs: List[T]): List[T] = xs match {
    case List() => throw new Error("index out of bounds")
    case List(x) => if (n == 0) List() else throw new Error("index out of bounds")
    case x :: xs => if (n == 0) xs else List(x) ++ removeAt_2(n-1,xs)
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (List(), ys) => ys
      case (xs, List()) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1,ys)
        else y :: merge(xs,ys1)
    }
  /*
  def encode[T](xs: List[T]): List[T] = {
    val packed = pack(xs)
    packed match {
      case Nil => xs
      case x :: xs1 =>
    }
  }
   */
}
