abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

//foldleft trial
val const = 3
val x: List[Int] = List(1,2,3,4,5,6)
val sum = x.foldLeft(const)((e1,e2) => e1+e2)
assert(sum == const + 21)

//times
def times(chars: List[Char]): List[(Char, Int)] = {
  def timesCount(chars: List[Char], counter: Int = 1): List[(Char, Int)] =
    chars.sorted match {
      case Nil => Nil
      case head :: Nil => List((head,counter))
      case head :: mid :: tail =>
        if (head == mid) timesCount(mid :: tail,counter+1)
        else (head,counter) :: timesCount(mid :: tail)
    }
  timesCount(chars)
}

val list = List('a','b','a','c','b','b','e')
val a = times(list)

//makeOrderedLeafList
def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
  freqs.sortWith((pair1,pair2) => pair1._2 <= pair2._2) match {
    case Nil => Nil
    case head :: tail => Leaf(head._1,head._2) :: makeOrderedLeafList(tail)
  }
  //freqs.sortWith((pair1,pair2) => pair1._2 <= pair2._2).map(pair => Leaf(pair._1,pair._2))

val b = makeOrderedLeafList(a)